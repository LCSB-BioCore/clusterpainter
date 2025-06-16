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
import Data.Bool (bool)
import qualified Data.ByteString.Internal as BSI
import Data.Char (chr, ord)
import Data.Foldable (foldl', for_)
import Data.IORef
import Data.List (foldl1')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
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
      sc = min (1 / xs) (1 / ys) * 0.9 --TODO zoom?
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
       in bump
            (if hc > 3
               then hc - 6
               else hc)
    bump = min 1 . max 0 . (+ negate 1) . abs
    ub = v
    lb = ub * (1 - s)
    scale = ub - lb
    h6 = h * 6

{- rendering (low-level) -}
vertexShaderWithCoords =
  "#version 430 core\n\
 \ layout (location = 0) in vec3 pos;\n\
 \ uniform mat4 proj;\n\
 \ uniform float size;\n\
 \ uniform vec2 trans;\n\
 \ out vec2 in_coord;\n\
 \ void main()\n\
 \ {\n\
 \    gl_Position = proj*vec4(\n\
 \      trans.x+size*pos.x,\n\
 \      trans.y+size*pos.y,\n\
 \      pos.z, 1.0);\n\
 \    in_coord = pos.xy;\n\
 \ }"

fragmentShaderFlatColor =
  "#version 430 core\n\
 \ #define M_PI 3.1415926535897932384626433832795\n\
 \ out vec4 FragColor;\n\
 \ uniform vec4 color;\n\
 \ in vec2 in_coord;\n\
 \ void main()\n\
 \ {\n\
 \    if(in_coord.x*in_coord.x+in_coord.y*in_coord.y >= 1) discard;\n\
 \    FragColor = color;\n\
 \ }"

fragmentShaderStar =
  "#version 430 core\n\
 \ #define M_PI 3.1415926535897932384626433832795\n\
 \ out vec4 FragColor;\n\
 \ uniform int slices;\n\
 \ layout(std430, binding=0) buffer colorLayout { float colors[]; }; \n\
 \ layout(std430, binding=1) buffer sizeLayout { float sizes[]; }; \n\
 \ in vec2 in_coord;\n\
 \ void main()\n\
 \ {\n\
 \    int slice = int(slices*(atan(in_coord.y, in_coord.x)/(2*M_PI)+0.5));\n\
 \    if(slice < 0 || slice >= slices) discard;\n\
 \    if(in_coord.x*in_coord.x+in_coord.y*in_coord.y >= sizes[slice]*sizes[slice]) discard;\n\
 \    float ub = 0.9;\n\
 \    float lb = ub * 0.1;\n\
 \    vec3 col = vec3(colors[slice])*6-vec3(0,2,4);\n\
 \    col = vec3(col.x>3 ? col.x-6 : col.x, col.y>3 ? col.y-6 : col.y, col.z>3 ? col.z-6 : col.z);\n\
 \    col = max(min((abs(col)-1), 1.0), 0.0)*(ub-lb)+lb;\n\
 \    FragColor = vec4(col.x,col.y,col.z, 1.0);\n\
 \ }"

c2i = fromIntegral . ord

i2c = chr . fromIntegral

quadBuf = [-1, -1, -1, 1, 1, -1, 1, 1]

circleBufStepDeg = 10

circleBufSteps = 360 `div` circleBufStepDeg

ratio2rad = (2 * pi *)

makeShaderProgram vertexShader fragmentShader = do
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
  pure prog

renderSetup st = do
  -- shaders first
  progFlat <- makeShaderProgram vertexShaderWithCoords fragmentShaderFlatColor
  modifyIORef st $ rendererData . rdFlatProgram .~ progFlat
  progStar <- makeShaderProgram vertexShaderWithCoords fragmentShaderStar
  modifyIORef st $ rendererData . rdStarProgram .~ progStar
  -- prepare shader helpers
  let getUniformLocation prog name =
        withCString name $ glGetUniformLocation prog
  for_
    [ (progFlat, flatProjection, flatSize, flatPos)
    , (progStar, starProjection, starSize, starPos)
    ] $ \(prog, setProj, setSize, setPos) -> do
    ul <- getUniformLocation prog "proj"
    modifyIORef st
      $ rendererData . setProj
          .~ flip withArray (glUniformMatrix4fv ul 1 GL_FALSE)
    ul <- getUniformLocation prog "size"
    modifyIORef st $ rendererData . setSize .~ glUniform1f ul
    ul <- getUniformLocation prog "trans"
    modifyIORef st $ rendererData . setPos .~ glUniform2f ul
  ul <- getUniformLocation progFlat "color"
  modifyIORef st $ rendererData . flatColor .~ glUniform4f ul
  ul <- getUniformLocation progStar "slices"
  modifyIORef st $ rendererData . starSlices .~ glUniform1i ul . fromIntegral
  [colbuf, sizebuf] <-
    withArray [0, 0] $ \a -> glGenBuffers 2 a >> peekArray 2 a
  for_ [(starColors, colbuf, 0), (starSizes, sizebuf, 1)] $ \(what, buf, bid) ->
    modifyIORef st
      $ rendererData . what .~ \xs -> do
          glBindBuffer GL_SHADER_STORAGE_BUFFER buf
          withArrayLen xs $ \n a ->
            for_ [nullPtr, castPtr a] $ \ptr ->
              glBufferData
                GL_SHADER_STORAGE_BUFFER
                (4 * fromIntegral n)
                ptr
                GL_STREAM_DRAW
            -- explicitly respecify the buffer
          glBindBufferBase GL_SHADER_STORAGE_BUFFER bid buf
  -- array&data
  [arr] <- withArray [0] $ \a -> glGenVertexArrays 1 a >> peekArray 1 a
  [buf] <- withArray [0] $ \a -> glGenBuffers 1 a >> peekArray 1 a
  glBindVertexArray arr
  glBindBuffer GL_ARRAY_BUFFER buf
  withArrayLen (quadBuf :: [Float]) $ \n a ->
    glBufferData GL_ARRAY_BUFFER (4 * fromIntegral n) (castPtr a) GL_STATIC_DRAW
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 8 nullPtr
  glEnableVertexAttribArray 0
  modifyIORef st $ rendererData . rdCircleArray .~ arr
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
  glBindVertexArray (st ^. rendererData . rdCircleArray)
  let setupProgram which proj = do
        glUseProgram (st ^. rendererData . which)
        isotropicUnitProjection (st ^. rendererData . proj) sz
  let (cPos', cSz', _) = scaleUnscale st
      fPos = cPos' (st ^. rendererData . flatPos)
      fSz = cSz' (st ^. rendererData . flatSize)
      fCl = st ^. rendererData . flatColor
      sPos = cPos' (st ^. rendererData . starPos)
      sSz = cSz' (st ^. rendererData . starSize)
      sN = st ^. rendererData . starSlices
      sSzs = st ^. rendererData . starSizes
      sCls = st ^. rendererData . starColors
      maxWScale = recip . max 1e-3 . maximum $ st ^.. clusters . each . weight
  {- selection painting -}
  setupProgram rdFlatProgram flatProjection
  fSz 0.56
  fCl 0.222 0.222 0.222 1
  for_ (st ^.. clusters . to V.toList . each) $ \c ->
    when (c ^. clusterSelected) $ do
      v2rry fPos $ c ^. position
      glDrawArrays GL_TRIANGLE_STRIP 0 4
  {- cluster painting helpers -}
  let (featmap, groupmap) = featureGroupColors st id
      nFeats = M.size featmap
      nGroups = M.size groupmap
      franges = st ^. featureRanges
      wScale c =
        if (st ^. showWeights)
          then sqrt $ maxWScale * (c ^. weight)
          else 1.0
  {- groups -}
  when (not $ M.null groupmap) $ do
    setupProgram rdStarProgram starProjection
    sN nGroups
    sCls (M.elems groupmap)
    for_ (st ^.. clusters . each) $ \c -> do
      v2rry sPos $ c ^. position
      sSz $ wScale c * 0.4 + 0.08
      sSzs . map (\gid -> c ^. groups . to (bool 0 1 . S.member gid))
        $ M.keys groupmap
      glDrawArrays GL_TRIANGLE_STRIP 0 4
  {- sleepwalk background -}
  setupProgram rdFlatProgram flatProjection
  for_ (st ^.. clusters . each) $ \c -> do
    fSz $ wScale c * 0.4
    v2rry fPos $ c ^. position
    let clr =
          case swDist st c of
            Nothing -> 1
            Just dist
              | st ^. hover == Nothing -> 1
              | st ^. swMode == SWOff -> 1
              | st ^. swSelect -> st ^. swSigma . to (dist <=) . to (bool 1 0)
              | otherwise -> 1 - exp (-dist / (st ^. swSigma) ^ 2)
    fCl clr clr clr 1
    glDrawArrays GL_TRIANGLE_STRIP 0 4
  {- star plots -}
  when (not $ M.null featmap) $ do
    setupProgram rdStarProgram starProjection
    sN nFeats
    sCls (M.elems featmap)
    for_ (st ^.. clusters . each) $ \c -> do
      v2rry sPos $ c ^. position
      sSz $ wScale c * 0.35
      sSzs
        . map
            (\fid ->
               let (V2 fmin frng) = franges V.! fid
                in sqrt $ ((c ^. features . to (V.! fid) - fmin) / frng))
        $ M.keys featmap
      glDrawArrays GL_TRIANGLE_STRIP 0 4

{- event processing -}
swDist st c =
  case (st ^. hover, st ^. swMode) of
    (Just ci, SWTopo) -> c ^? topoDists . ix ci
    (Just ci, SWAllFeatures) ->
      let otherFs = st ^? clusters . ix ci . features
       in V.sum . V.zipWith (\a b -> (a - b) ^ 2) (c ^. features) <$> otherFs
    (Just ci, SWSelFeatures) ->
      let otherFs = st ^? clusters . ix ci . features
          sels = st ^. hiFeatures
       in V.sum
            . V.izipWith
                (\i a b ->
                   if S.member i sels
                     then (a - b) ^ 2
                     else 0)
                (c ^. features)
            <$> otherFs
    (_, _) -> Nothing

doPaint ci appst =
  unRef appst $ \st ->
    let sel =
          if st ^. swSelect
            then each . filtered (maybe False (<= st ^. swSigma) . swDist st)
            else ix ci
     in case st ^. painting of
          Just b -> modifyIORef appst $ clusters . sel . clusterSelected .~ b
          _ -> pure ()

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

featureGroupColors :: AppState -> (Float -> a) -> (M.Map Int a, M.Map Int a)
featureGroupColors st tr =
  let nFeats = st ^. hiFeatures . to S.size
      nGroups = st ^. hiGroups . to S.size
      n = nFeats + nGroups
      cols = map tr [fromIntegral i / fromIntegral n | i <- [1 .. n]]
      (groupCols, featCols) = bresenhamSplit nFeats n cols
   in ( M.fromAscList $ zip (st ^. hiFeatures . to S.toList) featCols
      , M.fromAscList $ zip (st ^. hiGroups . to S.toList) groupCols)

featureGroupColorsHSV ::
     AppState -> (M.Map Int (V4 Float), M.Map Int (V4 Float))
featureGroupColorsHSV st = featureGroupColors st (\h -> hsv2rgb h 0.9 0.9 1)

colorMarker col =
  textColored ((pure :: ImVec4 -> IO ImVec4) $ v4rry ImVec4 col) "■"

removeSetIndex idx set =
  S.fromAscList
    [ if i > idx
      then pred i
      else i
    | i <- S.toAscList set
    , i /= idx
    ]

drawUI _ appst = do
  st <- readIORef appst
  let (featmap, groupmap) = featureGroupColorsHSV st
      fSz = st ^. fontSize
      withWidth = withItemWidth . (fSz *)
  withWindowOpen "FPS" $ framerate >>= text . pack . show
  withWindowOpen "Features" $ do
    withZoom appst showWeights $ checkbox "Scale by weights"
    withZoom appst featureNames $ \r ->
      withVal_ r
        $ V.imapM
        $ \i t ->
            withRef_ t $ \r ->
              withID (pack $ show i)
                . withStyleColor
                    ImGuiCol_Button
                    (pure . v4rry ImVec4
                       $ M.findWithDefault (V4 0.8 0.8 0.8 1.0) i featmap :: IO
                       ImVec4) $ do
                whenM (button "    ") . modifyIORef appst
                  $ hiFeatures %~ runIdentity . S.alterF (pure . not) i
                sameLine
                withWidth (-1) $ inputText "" r 100
  withWindowOpen "Groups" $ do
    whenM (button "new group##at begin") . modifyIORef appst
      $ (groupNames %~ V.cons "group")
          . (hiGroups %~ S.mapMonotonic succ)
          . (clusters . each . groups %~ S.mapMonotonic succ)
    when (st ^. syncOutFile . to isJust) $ do
      sameLine
      whenM (button "export to file") $ doOutput st
    todel <-
      withRef_ Nothing $ \todel -> do
        withZoom appst groupNames . flip withVal_ . V.imapM $ \i t ->
          withRef_ t $ \r ->
            withID (pack $ show i)
              . withStyleColor
                  ImGuiCol_Button
                  (pure . v4rry ImVec4
                     $ M.findWithDefault (V4 0.8 0.8 0.8 1.0) i groupmap :: IO
                     ImVec4) $ do
              whenM (button "    ") $ do
                modifyIORef appst
                  $ hiGroups %~ runIdentity . S.alterF (pure . not) i
              sameLine
              -- TODO the button widths are somewhat WTH
              withWidth (-12) $ inputText "" r 100
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
                writeIORef todel (Just i)
    when (st ^. groupNames . to (not . V.null))
      . whenM (button "new group##at end")
      . modifyIORef appst
      $ groupNames %~ flip V.snoc "group"
    case todel of
      Nothing -> pure ()
      Just gid ->
        modifyIORef appst
          $ (hiGroups %~ removeSetIndex gid)
              . (clusters . each . groups %~ removeSetIndex gid)
              . (groupNames %~ V.ifilter (\i _ -> gid /= i))
  withWindowOpen "Neighborhoods" $ do
    text "Mode"
    let swm = st ^. swMode
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
            "%0.3g"
            ImGuiSliderFlags_Logarithmic
    withZoom appst swSelect $ void . checkbox "Select by neighborhood"
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
                          . filtered
                              (case gid of
                                 Just gid -> (^. groups . to (S.member gid))
                                 Nothing -> (^. clusterSelected))
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
            colorMarker (featmap M.! fid)
        when (or $ st ^.. clusters . each . clusterSelected) $ do
          tableNextRow
          tableNextColumn $ text "Selection"
          for_ (st ^. hiFeatures . to S.toAscList) $ \fid ->
            tableNextColumn
              $ plotLines (pack $ "##selection," ++ show fid)
              $ map CFloat
              $ plotData fid Nothing
        for_ (st ^. hiGroups . to S.toAscList) $ \gid -> do
          tableNextRow
          let gname = st ^. groupNames . to (V.! gid)
          tableNextColumn $ do
            text gname
            sameLine
            colorMarker (groupmap M.! gid)
          for_ (st ^. hiFeatures . to S.toAscList) $ \fid ->
            tableNextColumn
              $ plotLines (pack $ "##" ++ show gid ++ "," ++ show fid)
              $ map CFloat
              $ plotData fid (Just gid)
