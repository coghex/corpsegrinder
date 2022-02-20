module Screep.InterShardMemory where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

getLocal ∷ InterShardMemory → Effect String
getLocal = runThisEffFn0 "getLocal"

setLocal ∷ InterShardMemory → String → Effect String
setLocal = runThisEffFn1 "setLocal"

getRemote ∷ InterShardMemory → String → Effect String
getRemote = runThisEffFn1 "getRemote"
