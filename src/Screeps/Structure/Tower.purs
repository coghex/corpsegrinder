module Screeps.Structure.Tower where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

store ∷ Tower → Store
store = unsafeField "store"

attack ∷ ∀ α. Tower → RoomObject α → Effect ReturnCode
attack = runThisEffFn1 "attack"

heal ∷ ∀ α. Tower → RoomObject α → Effect ReturnCode
heal = runThisEffFn1 "heal"

repair ∷ ∀ α. Tower → Structure α → Effect ReturnCode
repair = runThisEffFn1 "repair"
