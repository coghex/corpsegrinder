module Screeps.Structure.PowerBank where

import UPrelude
import Screeps.Data
import Screeps.FFI

power ∷ PowerBank → Int
power = unsafeField "power"

ticksToDecay ∷ PowerBank → Int
ticksToDecay = unsafeField "ticksToDecay"
