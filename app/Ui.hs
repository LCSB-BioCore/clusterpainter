{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ui where

import Lensref
import St

import Control.Applicative
import Control.Lens
import Control.Monad (void, when)
import Data.Bits
import Data.Char (chr, ord)
import Data.Foldable (foldl', for_)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import DearImGui
import qualified DearImGui.FontAtlas as F
import DearImGui.Internal.Text
import qualified DearImGui.Raw.Font.GlyphRanges as GR
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.GL
import SDL

{- projection helpers -}
v2rry f (V2 x y) = f x y

v3rry f (V3 x y z) = f x y z

v4rry f (V4 x y z w) = f x y z w

inf = 1 / 0 :: Float

scaleUnscale ::
     AppState
  -> (GLfloat -> GLfloat -> IO (), GLfloat -> IO (), V2 Float -> V2 Float)
scaleUnscale st =
  let (vmin, vmax) = st ^. positionRange
      vmid@(V2 vmidx vmidy) = (vmin + vmax) / 2
      s@(V2 xs ys) = vmax - vmin
      sc = min (1 / xs) (1 / ys) * 0.8 --TODO zoom?
   in ( \posx posy ->
          circlePos ((posx - vmidx) * sc + 0.5) ((posy - vmidy) * sc + 0.5)
      , \size -> circleSize (size * sc)
      , \pt -> (pt - pure 0.5) / pure sc + vmid)

clusterAt pos st =
  getArg . minimum
    $ Arg inf Nothing
        : [ Arg d (Just i)
          | (i, p) <-
              zip [0 ..] $ st ^.. clusters . to V.toList . each . position
          , let pp = p - pos
                d = sum (pp * pp)
          , d <= 1
          ]
  where
    getArg (Arg _ a) = a

{- coloring helpers -}
hsv2rgb h s v a =
  V4 (lb + scale * clr 0) (lb + scale * clr 2) (lb + scale * clr 4) a
  where
    clr off =
      let hc = h6 - off
       in if hc > 3
            then bump (hc - 6)
            else bump hc
    bump x =
      let a = abs x
       in if a < 1
            then 1
            else if a < 2
                   then 2 - a
                   else 0
    ub = v
    lb = ub * (1 - s)
    scale = ub - lb
    h6 = h * 6

{- rendering (low-level) -}
vertexShader =
  "#version 430 core\n\
 \ layout (location = 0) in vec3 pos;\n\
 \ layout (location = 1) uniform mat4 proj;\n\
 \ layout (location = 2) uniform float size;\n\
 \ layout (location = 3) uniform vec2 trans;\n\
 \ layout (location = 4) uniform vec2 rot;\n\
 \ void main()\n\
 \ {\n\
 \    gl_Position = proj*vec4(\n\
 \      trans.x+size*(pos.x*rot.x+pos.y*rot.y),\n\
 \      trans.y+size*(pos.y*rot.x-pos.x*rot.y),\n\
 \      pos.z, 1.0);\n\
 \ }"

fragmentShader =
  "#version 430 core\n\
 \ out vec4 FragColor;\n\
 \ layout (location = 5) uniform vec4 color;\n\
 \ void main()\n\
 \ {\n\
 \    FragColor = color;\n\
 \ }"

c2i = fromIntegral . ord

i2c = chr . fromIntegral

circleBuf step =
  [0, 0]
    ++ concatMap (liftA2 (:) cos $ pure . sin) (map deg2rad [0,step .. 360])

circleBufStepDeg = 10

circleBufSteps = 360 `div` circleBufStepDeg

deg2rad x = pi * x / 180

setProjection p = withArray p $ \arr -> glUniformMatrix4fv 1 1 GL_FALSE arr

circleSize = glUniform1f 2

circlePos = glUniform2f 3

circleRot = (glUniform2f 4 <$> cos <*> sin) . deg2rad

circleColor = glUniform4f 5

renderSetup st = do
  -- shaders first
  [vs, fs] <- traverse glCreateShader [GL_VERTEX_SHADER, GL_FRAGMENT_SHADER]
  for_ [(vs, vertexShader), (fs, fragmentShader)] $ \(s, src) ->
    withArray (map c2i src ++ [0]) $ \psrc ->
      withArray [psrc] $ \ppsrc -> do
        glShaderSource s 1 ppsrc nullPtr
        glCompileShader s
        [succ] <-
          withArray [0] $ \a ->
            glGetShaderiv s GL_COMPILE_STATUS a >> peekArray 1 a
        when (succ == 0) $ do
          log <-
            withArray (replicate 512 0) $ \a ->
              glGetShaderInfoLog s 512 nullPtr a >> peekArray 512 a
          print . map i2c $ takeWhile (/= 0) log
  prog <- glCreateProgram
  traverse (glAttachShader prog) [vs, fs]
  glLinkProgram prog
  traverse glDeleteShader [vs, fs]
  modifyIORef st $ rendererData . rdProgram .~ prog
  -- array&data
  [arr] <- withArray [0] $ \a -> glGenVertexArrays 1 a >> peekArray 1 a
  [buf] <- withArray [0] $ \a -> glGenBuffers 1 a >> peekArray 1 a
  glBindVertexArray arr
  glBindBuffer GL_ARRAY_BUFFER buf
  withArrayLen (circleBuf circleBufStepDeg :: [Float]) $ \n a ->
    glBufferData GL_ARRAY_BUFFER (4 * fromIntegral n) (castPtr a) GL_STATIC_DRAW
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 8 nullPtr
  glEnableVertexAttribArray 0
  modifyIORef st $ rendererData . rdCircleArr .~ arr
  -- rendering stuff
  glClearColor 0.2 0.2 0.2 1.0
  -- imgui
  F.clear
  builder <- GR.new
  for_ [GR.Latin, GR.Cyrillic] $ GR.addRanges builder . GR.getBuiltin
  -- ranges are: latin ext + IPA, and greek+coptic+cyrillic+ext
  withArray [0xa0, 0x2ff, 0x370, 0x52f, 0] $ \a ->
    GR.addRanges builder $ GR.GlyphRanges a
  withCString "■ □" $ GR.addText builder
  rangesVec <- GR.buildRangesVector builder
  let ranges = GR.fromRangesVector rangesVec
  --TODO carry the file
  F.addFontFromFileTTF
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    20.0
    Nothing
    (Just ranges)
  F.build
  GR.destroyRangesVector rangesVec
  GR.destroy builder

renderApp s appst = unRef appst $ renderApp' s

isotropicUnitProjection sz = do
  let V2 sx sy = fromIntegral <$> sz
  if sx >= sy
    then let r = sy / sx
          in setProjection
               [2 * r, 0, 0, 0, 0, -2, 0, 0, 0, 0, 1, 0, -r, 1, 0, 1]
    else let r = sx / sy
          in setProjection
               [2, 0, 0, 0, 0, -2 * r, 0, 0, 0, 0, 1, 0, -1, r, 0, 1]

unprojectIsotropicUnit sz pt = do
  let V2 sx sy = fromIntegral <$> sz
      V2 px py = fromIntegral <$> pt
  if sx >= sy
    then V2 (((px - (sx - sy) / 2) / sy)) (py / sy)
    else V2 (px / sx) ((py - (sy - sx) / 2) / sx)

renderApp' sz st = do
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  glUseProgram (st ^. rendererData . rdProgram)
  glBindVertexArray (st ^. rendererData . rdCircleArr)
  isotropicUnitProjection sz
  let cPos :: Float -> Float -> IO ()
      cSz :: Float -> IO ()
      (cPos, cSz, _) = scaleUnscale st
  circleRot 0
  {- selection painting -}
  cSz 0.666
  circleColor 0.6 0.6 0.6 1
  for_ (st ^.. clusters . to V.toList . each) $ \c ->
    when (c ^. clusterSelected) $ do
      v2rry cPos $ c ^. position
      glDrawArrays GL_TRIANGLE_FAN 0 (circleBufSteps + 2)
  {- cluster painting -}
  let (featmap, groupmap) = featureGroupColors st
      featAngle = 360 / fromIntegral (M.size featmap)
      nFeatSlices = ceiling $ featAngle / circleBufStepDeg
      franges = st ^. featureRanges
      groupAngle = 360 / fromIntegral (M.size groupmap)
      nGroupSlices = ceiling $ groupAngle / circleBufStepDeg
  for_ (st ^.. clusters . each) $ \c -> do
    v2rry cPos $ c ^. position
    {- groups -}
    cSz 0.48
    when (not $ M.null groupmap)
      $ for_ (zip [0 ..] (M.toAscList groupmap))
      $ \(i, (gid, clr)) ->
          when (c ^. groups . to (S.member gid)) $ do
            v4rry circleColor clr
            circleRot (groupAngle * i)
            glDrawArrays GL_TRIANGLE_FAN 0 (nGroupSlices + 2)
    circleRot 0
    {- sleepwalk background -}
    cSz 0.4
    case (st ^. hover, st ^. swMode) of
      (Just ci, SWTopo) ->
        let Just dist = c ^? topoDists . ix ci
            clr = exp $ -dist / (st ^. swSigma) ^ 2
         in circleColor clr clr clr 1
      (Just ci, SWAllFeatures) ->
        let Just otherFs = st ^? clusters . ix ci . features
            dist =
              V.sum $ V.zipWith (\a b -> (a - b) ^ 2) (c ^. features) (otherFs)
            clr = exp $ -dist / (st ^. swSigma) ^ 2
         in circleColor clr clr clr 1
      (Just ci, SWSelFeatures) ->
        let Just otherFs = st ^? clusters . ix ci . features
            sels = st ^. hiFeatures
            dist =
              V.sum
                $ V.izipWith
                    (\i a b ->
                       if S.member i sels
                         then (a - b) ^ 2
                         else 0)
                    (c ^. features)
                    (otherFs)
            clr = exp $ -dist / (st ^. swSigma) ^ 2
         in circleColor clr clr clr 1
      (_, _) -> circleColor 0 0 0 1
    glDrawArrays GL_TRIANGLE_FAN 0 (circleBufSteps + 2)
    {- star plot-}
    when (not $ M.null featmap) $ do
      for_ (zip [0 ..] (M.toAscList featmap)) $ \(i, (fid, clr)) -> do
        v4rry circleColor clr
        circleRot (featAngle * i)
        let (V2 fmin frng) = franges V.! fid
        cSz . (0.3 *) . sqrt $ ((c ^. features) V.! fid - fmin) / frng
        glDrawArrays GL_TRIANGLE_FAN 0 (nFeatSlices + 2)
    circleRot 0

doPaint ci appst =
  unRef appst $ \st -> do
    case st ^. painting of
      Just b -> modifyIORef appst $ clusters . ix ci . clusterSelected .~ b
      _ -> pure ()

{- event processing -}
onEvent sz (MouseButtonEvent b) appst
  | Released <- mouseButtonEventMotion b
  , ButtonLeft <- mouseButtonEventButton b =
    modifyIORef appst $ painting .~ Nothing
  | Pressed <- mouseButtonEventMotion b
  , ButtonLeft <- mouseButtonEventButton b
  , P p <- mouseButtonEventPos b =
    unRef appst $ \st ->
      case st ^. hover of
        Just ci -> do
          let Just action =
                st ^? clusters . ix ci . clusterSelected . to (Just . not)
          modifyIORef appst $ painting .~ action
          doPaint ci appst
        Nothing -> do
          modifyIORef appst $ clusters . each . clusterSelected .~ False
onEvent sz (MouseMotionEvent b) appst
  | P p <- mouseMotionEventPos b =
    unRef appst $ \st ->
      let (_, _, unscale) = scaleUnscale st
          ci' = clusterAt (unscale $ unprojectIsotropicUnit sz p) st
       in do
            modifyIORef appst $ hover .~ ci'
            case ci' of
              Just ci -> do
                doPaint ci appst
              _ -> pure ()
onEvent _ _ _ = pure ()

{- user interface -}
whenM a b = a >>= flip when b

bresenhamSplit k n xs
  | k * 2 > n =
    let (as, bs) = bresenhamSplit (n - k) n xs
     in (bs, as)
  | otherwise = go ((2 * k) - n) xs
  where
    go _ [] = ([], [])
    go d (x:xs)
      | d > 0 =
        let (as, bs) = go (d - 2 * n + 2 * k) xs
         in (as, x : bs)
      | otherwise =
        let (as, bs) = go (d + 2 * k) xs
         in (x : as, bs)

featureGroupColors :: AppState -> (M.Map Int (V4 Float), M.Map Int (V4 Float))
featureGroupColors st =
  let nFeats = st ^. hiFeatures . to S.size
      nGroups = st ^. hiGroups . to S.size
      n = nFeats + nGroups
      cols :: [V4 Float]
      cols =
        map
          (\h -> hsv2rgb h 0.75 1 1)
          [fromIntegral i / fromIntegral n | i <- [1 .. n]]
      (groupCols, featCols) = bresenhamSplit nFeats n cols
   in ( M.fromAscList $ zip (st ^. hiFeatures . to S.toList) featCols
      , M.fromAscList $ zip (st ^. hiGroups . to S.toList) groupCols)

drawUI _ appst = do
  (featmap, groupmap) <- featureGroupColors <$> readIORef appst
  --showDemoWindow
  withWindowOpen "FPS" $ framerate >>= text . pack . show
  withWindowOpen "Features"
    $ withZoom appst featureNames
    $ \r ->
        withVal_ r
          $ V.imapM
          $ \i t ->
              withRef_ t $ \r ->
                withID (pack $ show i) $ do
                  case featmap M.!? i of
                    Nothing -> text "□"
                    Just col ->
                      textColored
                        ((pure :: ImVec4 -> IO ImVec4) $ v4rry ImVec4 col)
                        "■"
                  sameLine
                  inputText "" r 100
                  sameLine
                  whenM (button "show") $ do
                    modifyIORef appst $ hiFeatures %~ S.insert i
                  sameLine
                  whenM (button "hide") $ do
                    modifyIORef appst $ hiFeatures %~ S.delete i
  withWindowOpen "Groups" $ do
    withZoom appst groupNames $ \r ->
      withVal_ r
        $ V.imapM
        $ \i t ->
            withRef_ t $ \r ->
              withID (pack $ show i) $ do
                case groupmap M.!? i of
                  Nothing -> text "□"
                  Just col ->
                    textColored
                      ((pure :: ImVec4 -> IO ImVec4) $ v4rry ImVec4 col)
                      "■"
                sameLine
                inputText "" r 100
                sameLine
                whenM (button "show") $ do
                  modifyIORef appst $ hiGroups %~ S.insert i
                sameLine
                whenM (button "hide") $ do
                  modifyIORef appst $ hiGroups %~ S.delete i
                sameLine
                whenM (button "select") $ do
                  modifyIORef appst
                    $ clusters . traverse
                        %~ (\c ->
                              clusterSelected .~ (c ^. groups . to (S.member i))
                                $ c)
                sameLine
                whenM (button "assign") $ do
                  modifyIORef appst $ hiGroups %~ S.insert i
                  modifyIORef appst
                    $ clusters . traverse
                        %~ (\c ->
                              c
                                & groups
                                    %~ (if c ^. clusterSelected
                                          then S.insert i
                                          else S.delete i))
                sameLine
                whenM (button "delete") $ do
                  pure () -- TODO
    whenM (button "add") $ do
      modifyIORef appst $ groupNames %~ flip V.snoc "group"
    --TODO permanent syncs
    whenM ((^. syncOutFile . to isJust) <$> readIORef appst)
      $ whenM (button "export to file")
      $ readIORef appst >>= doOutput
  withWindowOpen "Sleepwalk" $ do
    text "Mode"
    swm <- (^. swMode) <$> readIORef appst
    for_
      [ (SWOff, "Off")
      , (SWAllFeatures, "All features")
      , (SWSelFeatures, "Visible features")
      , (SWTopo, "Topology")
      ] $ \(val, lab) -> do
      whenM (radioButton lab $ swm == val) $ modifyIORef appst $ swMode .~ val
    text "Highlight distance"
    withZoom appst swSigma $ \swls ->
      void
        $ sliderScalar
            "##sigma"
            ImGuiDataType_Float
            swls
            (pure 1e-2 :: IO Float)
            (pure 1e2)
            "%0.3g σ"
            ImGuiSliderFlags_Logarithmic
  withWindowOpen "Data" $ do
    whenM (collapsingHeader "In selection" Nothing) $ do
      text "TODO"
    whenM (collapsingHeader "By groups" Nothing) $ do
      text "TODO"
    whenM (collapsingHeader "By feature" Nothing) $ do
      text "TODO"
