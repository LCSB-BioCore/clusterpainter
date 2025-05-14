{-# LANGUAGE OverloadedStrings #-}

module Ui where

import Lensref
import St

import Control.Monad (when)
import qualified Data.Vector as V
import DearImGui
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture hiding (text)
import Graphics.Gloss.Rendering
import SDL

renderApp xs ys appst = withVal appst $ \st -> pure (greyN 0.2, pic st)
  where
    pic st =
      Scale 30 30
        $ Pictures
        $ map
            (\c ->
               uncurry translate (position c)
                 $ color (greyN $ features c V.! 0)
                 $ circleSolid
                 $ weight c / 2)
        $ V.toList
        $ clusters st

onEvent (MouseButtonEvent b) _ = print b
onEvent _ _ = pure ()

drawUI appst = do
  showDemoWindow
  withWindowOpen "Data input" $ do
    text "Hello, ImGui!"
    button "clickety clicky" >>= \c -> when c $ putStrLn "Ow!"
    --void $ sliderInt "haha" (appst :: IORef Int) 0 100
  {-
  withWindowOpen "Display" $ do
    pure ()
  withWindowOpen "Clustering" $ do
    pure ()
  -}
