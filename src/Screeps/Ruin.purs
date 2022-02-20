module Screeps.Ruin where

import UPrelude
import Screeps.Data
import Screeps.FFI

destroyTime ∷ Ruin → Int
destroyTime = unsafeField "destroyTime"

id ∷ Ruin → Id Ruin
id = unsafeField "id"

store ∷ Ruin → Store
store = unsafeField "store"

structure ∷ ∀ α. Ruin → Structure α
structure = unsafeField "structure"

ticksToDecay ∷ Ruin → Int
ticksToDecay = unsafeField "ticksToDecay"
