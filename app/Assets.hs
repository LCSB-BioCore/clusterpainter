{-# LANGUAGE TemplateHaskell #-}

module Assets where

import Data.FileEmbed

uiFont = $(embedFileRelative "assets/DejaVuSans.ttf")
