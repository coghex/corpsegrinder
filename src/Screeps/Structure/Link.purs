module Screeps.Structure.Link where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

cooldown ∷ Link → Int
cooldown = unsafeField "cooldown"

store ∷ Link → Store
store = unsafeField "store"

transferEnergy ∷ Link → Link → Effect ReturnCode
transferEnergy = runThisEffFn1 "transferEnergy"

transferEnergy' ∷ Link → Link → Int → Effect ReturnCode
transferEnergy' = runThisEffFn2 "transferEnergy"
