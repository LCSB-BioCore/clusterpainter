{-# LANGUAGE TemplateHaskell #-}

module Assets where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)

uiFont :: ByteString
uiFont = $(embedFileRelative "assets/DejaVuSans.ttf")
