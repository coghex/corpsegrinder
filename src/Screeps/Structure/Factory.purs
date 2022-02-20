module Screeps.Structure.Factory where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

cooldown ∷ Factory → Int
cooldown = unsafeField "cooldown"

level ∷ Factory → Int
level = unsafeField "level"

store ∷ Factory → Store
store = unsafeField "store"

produce ∷ Factory → ResourceType → Effect ReturnCode
produce = runThisEffFn1 "produce"
