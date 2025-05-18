{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ui where

import Lensref
import St

import Control.Applicative
import Control.Lens
import Control.Monad (when)
import Data.Bits
import Data.Char (chr, ord)
import Data.Foldable (foldl', for_)
import Data.IORef
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import DearImGui
import DearImGui.Internal.Text
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.GL
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture hiding (text)
import Graphics.Gloss.Rendering
import SDL

{- projection helpers -}
v2rry f (V2 x y) = f x y

inf = 1 / 0 :: Float

projectionLimits =
  foldl'
    (\(l, u) c -> (liftA2 min l (c ^. position), liftA2 max u (c ^. position)))
    (pure inf, pure (-inf))

scaleUnscale ::
     (Integral i) => V2 i -> AppState -> (Picture -> Picture, V2 i -> V2 Float)
scaleUnscale ss@(V2 sx sy) st =
  let (vmin, vmax) = st ^. clusters . to projectionLimits
      s@(V2 xs ys) = vmax - vmin
      sc = min (fromIntegral sx / xs) (fromIntegral sy / ys) * 0.8 --TODO zoom
   in ( Scale sc (-sc) . Translate (-xs / 2) (-ys / 2)
      , \cur -> fmap fromIntegral (cur - (liftA2 div ss 2)) / pure sc + s / 2)

clusterAt pos sz st =
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
hsv2rgb h s v = (lb + scale * clr 0, lb + scale * clr 2, lb + scale * clr 4)
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

{-
hsvColor h s v = makeColor r g b 1.0
  where
    (r, g, b) = hsv2rgb h s v

picIf True x = x
picIf False _ = Blank

selColor = Color $ greyN 0.4
-}

{- rendering  OLD -}
{-
paintClusterSel sel = picIf sel $ selColor $ circleSolid 0.71

paintCluster gcols fcols bg sc =
  Pictures
    [ Scale sc sc
        $ Pictures
            [ let ng = length gcols
                  fng = fromIntegral ng
               in picIf (ng /= 0)
                    $ Pictures
                    $ do
                        (i, g) <- zip [0 ..] gcols
                        let fi = fromIntegral i
                        pure . Color g
                          $ ThickArc
                              (fi * 360 / fng)
                              ((fi + 1) * 360 / fng)
                              0.225
                              0.45
            , Color (greyN bg) $ circleSolid 0.4
            , let nf = length fcols
                  fnf = fromIntegral nf
               in picIf (nf /= 0)
                    $ Pictures
                    $ do
                        (i, f) <- zip [0 ..] fcols
                        let fi = fromIntegral i
                        pure . Color f
                          $ ThickArc
                              (fi * 360 / fnf)
                              ((fi + 1) * 360 / fnf)
                              0.15
                              0.3
            ]
    ]

renderApp'__ sz st =
  scale . Pictures
    $ map
        (\c ->
           v2rry translate (c ^. position)
             $ paintClusterSel (c ^. clusterSelected))
        cs
        ++ map
             (\c ->
                v2rry translate (c ^. position)
                  $ paintCluster
                      []
                      [ fcolor i (c ^. features . to (V.! i))
                      | i <- S.toList (st ^. hiFeatures)
                      ]
                      0.0
                      (c ^. weight))
             cs
  where
    cs = st ^. clusters . to V.toList
    nfeat = st ^. featureNames . to length . to fromIntegral
    fcolor fid fval = hsvColor (fromIntegral fid / nfeat) 1.0 fval
    (scale, _) = scaleUnscale sz st

renderApp__ s appst = unRef appst $ \st -> pure (greyN 0.2, renderApp' s st)

{- rendering utils -}
glPushPopMatrix a = glPushMatrix *> a <* glPopMatrix

glBeginEnd t a = glBegin t *> a <* glEnd
-}

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

circleBuf step = [0, 0]
        ++ concatMap
             (liftA2 (:) cos $ pure . sin)
             (map deg2rad [0,step .. 360])

circleBufStepDeg = 20

circleBufSteps = 360 `div` circleBufStepDeg

deg2rad x = pi*x/180

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

{-
renderArc degs = do
  glBeginEnd GL_TRIANGLE_FAN $ do
    glVertex2f 0 0
    for_ degs $ (glVertex2f <$> cos <*> sin) . (/ 180) . (* pi)

renderCircle = renderArc [0,20 .. 360]
-}

renderApp s appst = unRef appst $ renderApp' s

renderApp' sz st = do
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  glUseProgram (st ^. rendererData . rdProgram)
  glBindVertexArray (st ^. rendererData . rdCircleArr)
  setProjection [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]
  circleSize 0.5
  circlePos 0 0
  circleRot 0
  circleColor 0.5 0.6 0.7 1.0
  glDrawArrays GL_TRIANGLE_FAN 0 (circleBufSteps+2)
  circleSize 0.01
  for_ [0 .. 39] $ \i ->
    for_ [0 .. 39] $ \j -> do
      circlePos (i/20 - 1) (j/20 - 1)
      circleColor (i / 39) (j / 39) 0.8 1.0
      glDrawArrays GL_TRIANGLE_FAN 0 (circleBufSteps+2)
  {-glLoadIdentity
  glScalef 0.1 0.1 1
  for_ [0 .. 39] $ \i ->
    for_ [0 .. 39] $ \j -> do
      glColor3f (i / 19) (j / 19) 0.8
      glPushPopMatrix $ do
        glTranslatef i j 0
        glScalef 0.4 0.4 1
        renderCircle-}

{- event processing -}
onEvent sz (MouseButtonEvent b) appst
  | Pressed <- mouseButtonEventMotion b
  , ButtonLeft <- mouseButtonEventButton b
  , P p <- mouseButtonEventPos b =
    unRef appst $ \st ->
      let unscale = snd $ scaleUnscale sz st
       in case clusterAt (unscale p) sz st of
            Just ci -> do
              print ci
              modifyIORef appst $ clusters . ix ci . clusterSelected %~ not
            _ -> pure ()
onEvent _ _ _ = pure ()

{- user interface -}
whenM a b = a >>= flip when b

drawUI _ appst = do
  showDemoWindow
  withWindowOpen "FPS" $ framerate >>= text . pack . show
  withWindowOpen "Features"
    $ withZoom appst featureNames
    $ \r ->
        withVal_ r
          $ V.imapM
          $ \i t ->
              withRef_ t $ \r ->
                withID (pack $ show i) $ do
                  inputText "" r 100
                  sameLine
                  whenM (button "show") $ do
                    print i
                    modifyIORef appst $ hiFeatures %~ S.insert i
                  sameLine
                  whenM (button "hide") $ do
                    print i
                    modifyIORef appst $ hiFeatures %~ S.delete i
  withWindowOpen "Groups" $ text "TODO"
  withWindowOpen "Sleepwalk" $ text "TODO"
  withWindowOpen "Data" $ do
    whenM (collapsingHeader "In selection" Nothing) $ do
      text "TODO"
    whenM (collapsingHeader "By groups" Nothing) $ do
      text "TODO"
    whenM (collapsingHeader "By feature" Nothing) $ do
      text "TODO"
