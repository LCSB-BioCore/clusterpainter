{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ui where

import Lensref
import St

import Control.Applicative
import Control.Lens
import Control.Monad (when)
import Data.Foldable (foldl', for_)
import Data.IORef
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import DearImGui
import DearImGui.Internal.Text
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture hiding (text)
import Graphics.Gloss.Rendering
import SDL

import Debug.Trace

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

hsvColor h s v = makeColor r g b 1.0
  where
    (r, g, b) = hsv2rgb h s v

picIf True x = x
picIf False _ = Blank

selColor = Color $ greyN 0.4

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

{- rendering -}
renderApp' sz st =
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

renderApp s appst = unRef appst $ \st -> pure (greyN 0.2, renderApp' s st)

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
