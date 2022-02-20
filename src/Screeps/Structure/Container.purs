module Screeps.Structure.Container where

import UPrelude
import Screeps.Data
import Screeps.FFI

store ∷ Container → Store
store = unsafeField "store"

ticksToDecay ∷ Container → Int
ticksToDecay = unsafeField "ticksToDecay"
