{-# LANGUAGE TemplateHaskell #-}

module Assets where

import Data.ByteString
import Data.FileEmbed

uiFont :: ByteString
uiFont = $(embedFileRelative "assets/DejaVuSans.ttf")
