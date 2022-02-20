module Screeps.Store where

import UPrelude
import Screeps.Data
import Screeps.FFI

getCapacity ∷ Store → Int
getCapacity = runThisFn0 "getCapacity"

getCapacity' ∷ Store → ResourceType → Int
getCapacity' s (ResourceType res) = runThisFn1 "getCapacity" s res

getFreeCapacity ∷ Store → Int
getFreeCapacity = runThisFn0 "getFreeCapacity"

getFreeCapacity' ∷ Store → ResourceType → Int
getFreeCapacity' s (ResourceType res) = runThisFn1 "getFreeCapacity" s res

getUsedCapacity ∷ Store → Int
getUsedCapacity = runThisFn0 "getUsedCapacity"

getUsedCapacity' ∷ Store → ResourceType → Int
getUsedCapacity' s (ResourceType res) = runThisFn1 "getUsedCapacity" s res
