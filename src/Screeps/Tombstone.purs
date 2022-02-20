module Screeps.Tombstone where

import UPrelude
import Screeps.Data
import Screeps.FFI

-- TODO: unify these functions
creep ∷ Tombstone → Creep
creep = unsafeField "creep"

powerCreep ∷ Tombstone → Creep
powerCreep = unsafeField "creep"

deathTime ∷ Tombstone → Int
deathTime = unsafeField "deathTime"

id ∷ Tombstone → Id Tombstone
id = unsafeField "id"

store ∷ Tombstone → Store
store = unsafeField "store"

ticksToDecay ∷ Tombstone → Int
ticksToDecay = unsafeField "ticksToDecay"
