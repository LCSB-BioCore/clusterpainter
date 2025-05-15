{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ui where

import Lensref
import St

import Control.Monad (when)
import Data.Foldable (foldl', for_)
import Data.IORef
import qualified Data.Vector.Strict as V
import DearImGui
import DearImGui.Internal.Text
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture hiding (text)
import Graphics.Gloss.Rendering
import Lens.Micro
import SDL

import Debug.Trace

{- projection helpers -}
both2 f (a, b) (c, d) = (f a c, f b d)

projectionLimits =
  foldl'
    (\(l, u) c -> (both2 min l (c ^. position), both2 max u (c ^. position)))
    ((inf, inf), (-inf, -inf))
  where
    inf = 1 / 0 :: Float

scaleUnscale sx sy st =
  let ((xmin, ymin), (xmax, ymax)) = st ^. clusters . to projectionLimits
      xs = xmax - xmin
      ys = ymax - ymin
      sc = min (fromIntegral sx / xs) (fromIntegral sy / ys) * 0.8 --TODO zoom
   in (Scale sc sc . Translate (-xs / 2) (-ys / 2), \x y -> (0, 0))

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

hsvColor h s v = Color (makeColor r g b 1.0)
  where
    (r, g, b) = hsv2rgb h s v

{- rendering -}
renderApp' sx sy st =
  scale
    $ Pictures
    $ map
        (\c ->
           uncurry translate (c ^. position)
             $ hsvColor
                 (fromIntegral (st ^. hiFeature) / nfeat)
                 1.0
                 ((c ^. features) V.! (st ^. hiFeature))
             $ circleSolid
             $ (c ^. weight) * 0.45)
    $ V.toList
    $ _clusters st
  where
    nfeat = st ^. featureNames . to length . to fromIntegral
    (scale, _) = scaleUnscale sx sy st

renderApp sx sy appst =
  unRef appst $ \st -> pure (greyN 0.2, renderApp' sx sy st)

{- event processing -}
onEvent (MouseButtonEvent b) _ = print b
onEvent _ _ = pure ()

{- user interface -}
drawUI appst = do
  showDemoWindow
  withWindowOpen "FPS" $ framerate >>= text . pack . show
  withWindowOpen "Features"
    $ withZoom appst featureNames
    $ \r ->
        withVal_ r
          $ V.imapM
          $ \i t ->
              withRef_ t $ \r -> do
                inputText (pack $ "##" ++ show (i + 1)) r 100
                sameLine
                --TODO use withID
                button (pack $ "view##" ++ show i) >>= \b ->
                  when b $ do
                    print i
                    modifyIORef appst $ hiFeature .~ i
  withWindowOpen "Groups" $ text "TODO"
  withWindowOpen "Sleepwalk" $ text "TODO"
  withWindowOpen "Data" $ do
    collapsingHeader "In selection" Nothing >>= \b ->
      when b $ do
        text "TODO"
    collapsingHeader "By groups" Nothing >>= \b ->
      when b $ do
        text "TODO"
    collapsingHeader "By feature" Nothing >>= \b ->
      when b $ do
        text "TODO"
