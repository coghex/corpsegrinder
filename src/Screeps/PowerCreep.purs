module Screeps.PowerCreep where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

create ∷ PowerCreep → String → PowerClass → Effect ReturnCode
create = runThisEffFn2 "create"

-- TODO: implement the rest of this
