module Screeps.Structure.Extractor where

import UPrelude
import Screeps.Data
import Screeps.FFI

cooldown ∷ Extractor → Int
cooldown = unsafeField "cooldown"
