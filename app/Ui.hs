{-# LANGUAGE OverloadedStrings #-}

module Ui where

import Lensref
import St

import Control.Monad (when)
import DearImGui
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Rendering
import SDL

renderApp xs ys appst = pure $ (greyN 0.2, Circle 30)

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
