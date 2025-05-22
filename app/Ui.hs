{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ui where

import Assets
import Lensref
import St

import Control.Applicative
import Control.Lens
import Control.Monad (void, when)
import Data.Bits
import qualified Data.ByteString.Internal as BSI
import Data.Char (chr, ord)
import Data.Foldable (foldl', for_)
import Data.IORef
import Data.List (foldl1')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import DearImGui
import qualified DearImGui.FontAtlas as F
import DearImGui.Internal.Text
import qualified DearImGui.Raw.Font as RF
import qualified DearImGui.Raw.Font.Config as RFC
import qualified DearImGui.Raw.Font.GlyphRanges as GR
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.ForeignPtr
import Graphics.GL
import SDL

{- projection helpers -}
v2rry f (V2 x y) = f x y

v3rry f (V3 x y z) = f x y z

v4rry f (V4 x y z w) = f x y z w

inf = 1 / 0 :: Float

scaleUnscale ::
     AppState
  -> ( (Float -> Float -> a) -> Float -> Float -> a
     , (Float -> b) -> Float -> b
     , V2 Float -> V2 Float)
scaleUnscale st =
  let (vmin, vmax) = st ^. positionRange
      vmid@(V2 vmidx vmidy) = (vmin + vmax) / 2
      s@(V2 xs ys) = vmax - vmin
      sc = min (1 / xs) (1 / ys) * 0.8 --TODO zoom?
   in ( \cp posx posy ->
          cp ((posx - vmidx) * sc + 0.5) ((posy - vmidy) * sc + 0.5)
      , \cs size -> cs (size * sc)
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
  "#version 330 core\n\
 \ layout (location = 0) in vec3 pos;\n\
 \ uniform mat4 proj;\n\
 \ uniform float size;\n\
 \ uniform vec2 trans;\n\
 \ uniform vec2 rot;\n\
 \ void main()\n\
 \ {\n\
 \    gl_Position = proj*vec4(\n\
 \      trans.x+size*(pos.x*rot.x+pos.y*rot.y),\n\
 \      trans.y+size*(pos.y*rot.x-pos.x*rot.y),\n\
 \      pos.z, 1.0);\n\
 \ }"

fragmentShader =
  "#version 330 core\n\
 \ out vec4 FragColor;\n\
 \ uniform vec4 color;\n\
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
  -- prepare shader helpers
  ul <- withCString "proj" $ glGetUniformLocation prog
  modifyIORef st
    $ rendererData . setProjection
        .~ flip withArray (glUniformMatrix4fv ul 1 GL_FALSE)
  print ul
  ul <- withCString "size" $ glGetUniformLocation prog
  modifyIORef st $ rendererData . circleSize .~ glUniform1f ul
  print ul
  ul <- withCString "trans" $ glGetUniformLocation prog
  modifyIORef st $ rendererData . circlePos .~ glUniform2f ul
  print ul
  ul <- withCString "rot" $ glGetUniformLocation prog
  modifyIORef st
    $ rendererData . circleRot .~ (glUniform2f ul <$> cos <*> sin) . deg2rad
  print ul
  ul <- withCString "color" $ glGetUniformLocation prog
  modifyIORef st $ rendererData . circleColor .~ glUniform4f ul
  print ul
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
  glClearColor 0.9 0.9 0.9 1.0
  -- imgui
  styleColorsLight
  F.clear
  builder <- GR.new
  for_ [GR.Latin, GR.Cyrillic] $ GR.addRanges builder . GR.getBuiltin
  -- ranges are: latin ext + IPA, and greek+coptic+cyrillic+ext
  withArray [0xa0, 0x2ff, 0x370, 0x52f, 0] $ \a ->
    GR.addRanges builder $ GR.GlyphRanges a
  withCString "■ □" $ GR.addText builder
  rangesVec <- GR.buildRangesVector builder
  let ranges = GR.fromRangesVector rangesVec
      (fptr, flen) = BSI.toForeignPtr0 uiFont
  fsz <- (^. fontSize . to CFloat) <$> readIORef st
  fconf <- RFC.new
  RFC.setFontDataOwnedByAtlas fconf $ CBool 0
  withForeignPtr fptr $ \ptr ->
    RF.addFontFromMemoryTTF (castPtr ptr, flen) fsz fconf ranges
  F.build
  GR.destroyRangesVector rangesVec
  RFC.destroy fconf
  GR.destroy builder

renderApp s appst = unRef appst $ renderApp' s

isotropicUnitProjection sp sz = do
  let V2 sx sy = fromIntegral <$> sz
  if sx >= sy
    then let r = sy / sx
          in sp [2 * r, 0, 0, 0, 0, -2, 0, 0, 0, 0, 1, 0, -r, 1, 0, 1]
    else let r = sx / sy
          in sp [2, 0, 0, 0, 0, -2 * r, 0, 0, 0, 0, 1, 0, -1, r, 0, 1]

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
  isotropicUnitProjection (st ^. rendererData . setProjection) sz
  let cPos :: Float -> Float -> IO ()
      cSz :: Float -> IO ()
      (cPos', cSz', _) = scaleUnscale st
      cPos = cPos' (st ^. rendererData . circlePos)
      cSz = cSz' (st ^. rendererData . circleSize)
      maxWScale = recip . max 1e-3 . maximum $ st ^.. clusters . each . weight
      cRt = st ^. rendererData . circleRot
      cCl = st ^. rendererData . circleColor
  cRt 0
  {- selection painting -}
  cSz 0.666
  cCl 0.666 0.666 0.666 1
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
    let wScale =
          if (st ^. showWeights)
            then sqrt $ maxWScale * (c ^. weight)
            else 1.0
        cSzW = cSz . (wScale *)
    v2rry cPos $ c ^. position
    {- groups -}
    cSzW 0.48
    when (not $ M.null groupmap)
      $ for_ (zip [0 ..] (M.toAscList groupmap))
      $ \(i, (gid, clr)) ->
          when (c ^. groups . to (S.member gid)) $ do
            v4rry cCl clr
            cRt (groupAngle * i)
            glDrawArrays GL_TRIANGLE_FAN 0 (nGroupSlices + 2)
    cRt 0
    {- sleepwalk background -}
    cSzW 0.4
    case (st ^. hover, st ^. swMode) of
      (Just ci, SWTopo) ->
        let Just dist = c ^? topoDists . ix ci
            clr = 1 - exp (-dist / (st ^. swSigma) ^ 2)
         in cCl clr clr clr 1
      (Just ci, SWAllFeatures) ->
        let Just otherFs = st ^? clusters . ix ci . features
            dist =
              V.sum $ V.zipWith (\a b -> (a - b) ^ 2) (c ^. features) (otherFs)
            clr = 1 - exp (-dist / (st ^. swSigma) ^ 2)
         in cCl clr clr clr 1
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
            clr = 1 - exp (-dist / (st ^. swSigma) ^ 2)
         in cCl clr clr clr 1
      (_, _) -> cCl 1 1 1 1
    glDrawArrays GL_TRIANGLE_FAN 0 (circleBufSteps + 2)
    {- star plot-}
    when (not $ M.null featmap) $ do
      for_ (zip [0 ..] (M.toAscList featmap)) $ \(i, (fid, clr)) -> do
        v4rry cCl clr
        cRt (featAngle * i)
        let (V2 fmin frng) = franges V.! fid
        cSzW . (0.3 *) . sqrt $ ((c ^. features) V.! fid - fmin) / frng
        glDrawArrays GL_TRIANGLE_FAN 0 (nFeatSlices + 2)
    cRt 0

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
          (\h -> hsv2rgb h 0.9 0.9 1)
          [fromIntegral i / fromIntegral n | i <- [1 .. n]]
      (groupCols, featCols) = bresenhamSplit nFeats n cols
   in ( M.fromAscList $ zip (st ^. hiFeatures . to S.toList) featCols
      , M.fromAscList $ zip (st ^. hiGroups . to S.toList) groupCols)

colorMarker cmap i =
  case cmap M.!? i of
    Nothing -> text "□"
    Just col ->
      textColored ((pure :: ImVec4 -> IO ImVec4) $ v4rry ImVec4 col) "■"

drawUI _ appst = do
  st <- readIORef appst
  let (featmap, groupmap) = featureGroupColors st
  withWindowOpen "FPS" $ framerate >>= text . pack . show
  withWindowOpen "Features" $ do
    withZoom appst showWeights $ checkbox "Scale by weights"
    withZoom appst featureNames $ \r ->
      withVal_ r
        $ V.imapM
        $ \i t ->
            withRef_ t $ \r ->
              withID (pack $ show i) $ do
                colorMarker featmap i
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
                colorMarker groupmap i
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
    when (st ^. syncOutFile . to isJust)
      $ whenM (button "export to file")
      $ doOutput st
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
  withWindowOpen "Data"
    $ withTable defTableOptions "##data" (succ $ st ^. hiFeatures ^. to S.size)
    $ flip when
    $ do
        --TODO globalize the lows/ups I guess
        --TODO the multilens in the middle is called "alongside"
        let [flows, fhighs] =
              map
                (\f ->
                   zipWith
                     (V.zipWith f)
                     (st ^.. clusters . each . featMeans)
                     (st ^.. clusters
                        . each
                        . featVars
                        . to (V.map ((4 *) . sqrt))))
                [(-), (+)]
            fmins = foldl1' min flows
            fmaxs = foldl1' max fhighs
            plotData fid gid =
              let lb = fmins V.! fid
                  rng = fmaxs V.! fid - lb
                  xs = map ((+ lb) . (* rng)) [0,0.05 .. 1]
                  gaussContrib x m v w = w * exp ((x - m) ^ 2 / negate v)
                  contribAt x =
                    sum
                      $ st ^.. clusters
                          . each
                          . filtered (^. groups . to (S.member gid))
                          . to
                              (\c ->
                                 gaussContrib
                                   x
                                   (c ^. featMeans . to (V.! fid))
                                   (c ^. featVars . to (V.! fid))
                                   (c ^. weight))
               in map contribAt xs
        tableNextColumn $ text "Group \\ Feature"
        for_ (st ^. hiFeatures . to S.toAscList) $ \fid ->
          tableNextColumn $ do
            text (st ^. featureNames . to (V.! fid))
            sameLine
            colorMarker featmap fid
        --TODO selection
        for_ (st ^. hiGroups . to S.toAscList) $ \gid -> do
          tableNextRow
          let gname = st ^. groupNames . to (V.! gid)
          tableNextColumn $ do
            text gname
            sameLine
            colorMarker groupmap gid
          for_ (st ^. hiFeatures . to S.toAscList) $ \fid ->
            tableNextColumn
              $ plotLines (pack $ "##" ++ show gid ++ "," ++ show fid)
              $ map CFloat
              $ plotData fid gid
