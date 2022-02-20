module Screeps.Structure.Nuker where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

cooldown ∷ Nuker → Int
cooldown = unsafeField "cooldown"

store ∷ Nuker → Store
store = unsafeField "store"

launchNuke ∷ Nuker → RoomPosition → Effect ReturnCode
launchNuke = runThisEffFn1 "launchNuke"
