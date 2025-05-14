{-# LANGUAGE OverloadedStrings #-}

module Ui where

import Lensref
import St

import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef
import qualified Data.Vector as V
import DearImGui
import DearImGui.Internal.Text
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture hiding (text)
import Graphics.Gloss.Rendering
import Lens.Micro
import SDL

renderApp xs ys appst = unRef appst $ \st -> pure (greyN 0.2, pic st)
  where
    pic st =
      Scale 30 30
        $ Pictures
        $ map
            (\c ->
               uncurry translate (c ^. position)
                 $ color (greyN $ (c ^. features) V.! (st ^. hiFeature))
                 $ circleSolid
                 $ (c ^. weight) * 0.4)
        $ V.toList
        $ _clusters st

onEvent (MouseButtonEvent b) _ = print b
onEvent _ _ = pure ()

drawUI appst = do
  -- showDemoWindow
  withWindowOpen "Features"
    $ withZoom appst featureNames
    $ \r ->
        withVal_ r
          $ V.imapM
          $ \i t ->
              withRef_ t $ \r -> do
                inputText (pack $ "##" ++ show (i + 1)) r 100
                sameLine
                button (pack $ "view##" ++ show i) >>= \b ->
                  when b $ do
                    print i
                    modifyIORef appst $ hiFeature .~ i
