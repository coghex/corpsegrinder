module Screeps.Structure.Spawn where

import UPrelude
import Screeps.Data
import Screeps.FFI

ticksToDecay ∷ Spawn → Int
ticksToDecay = unsafeField "ticksToDecay"
