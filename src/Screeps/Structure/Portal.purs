module Screeps.Structure.Portal where

import UPrelude
import Screeps.Data
import Screeps.FFI

-- TODO: unify these functions
destination ∷ Portal → RoomPosition
destination = unsafeField "destination"

destination' ∷ Portal → { shard ∷ String, room ∷ String }
destination' = unsafeField "destination"

ticksToDecay ∷ Portal → Int
ticksToDecay = unsafeField "ticksToDecay"
