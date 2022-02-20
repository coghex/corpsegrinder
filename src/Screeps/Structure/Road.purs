module Screeps.Structure.Road where

import UPrelude
import Screeps.Data
import Screeps.FFI

ticksToDecay ∷ Road → Int
ticksToDecay = unsafeField "ticksToDecay"
