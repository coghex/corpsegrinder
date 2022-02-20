module Screeps.Structure.PowerSpawn where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

store ∷ PowerSpawn → Store
store = unsafeField "store"

processPower ∷ PowerSpawn → Effect ReturnCode
processPower = runThisEffFn0 "processPower"
