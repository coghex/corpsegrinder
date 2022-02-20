module Screeps.Game where

import UPrelude
import Effect (Effect)
import Data.Maybe (Maybe)
import Data.Map as M
import Foreign.Object as F
import Screeps.Data
import Screeps.FFI

foreign import getGameGlobal   ∷ Effect GameGlobal

constructionSites ∷ GameGlobal → F.Object ConstructionSite
constructionSites = unsafeField "constructionSites"

cpu ∷ GameGlobal → Cpu
cpu = unsafeField "cpu"

creeps ∷ GameGlobal → F.Object Creep
creeps = unsafeField "creeps"

flags ∷ GameGlobal → F.Object Flag
flags = unsafeField "flags"

gcl ∷ GameGlobal → Gcl
gcl = unsafeField "gcl"

gpl ∷ GameGlobal → Gpl
gpl = unsafeField "gpl"

map ∷ GameGlobal → WorldMap
map = unsafeField "map"

market ∷ GameGlobal → Market
market = unsafeField "market"

powerCreeps ∷ GameGlobal → F.Object PowerCreep
powerCreeps = unsafeField "powerCreeps"

resources ∷ GameGlobal → M.Map ResourceType Int
resources = unsafeField "resources"

rooms ∷ GameGlobal → F.Object Room
rooms = unsafeField "rooms"

shard ∷ GameGlobal → Shard
shard = unsafeField "shard"

spawns ∷ GameGlobal → F.Object Spawn
spawns = unsafeField "spawns"

structures ∷ ∀ α. GameGlobal → M.Map (Id (Structure α)) (Structure α)
structures = unsafeField "structures"

time ∷ GameGlobal → Int
time = unsafeField "time"

getHeapStatistics ∷ GameGlobal → Effect HeapStatistics
getHeapStatistics game = runThisEffFn0 "getHeapStatistics" (cpu game)

getUsed ∷ GameGlobal → Effect Number
getUsed game = runThisEffFn0 "getUsed" (cpu game)

halt ∷ GameGlobal → Effect Unit
halt game = runThisEffFn0 "halt" (cpu game)

setShardLimits ∷ GameGlobal → F.Object Int → Effect ReturnCode
setShardLimits game = runThisEffFn1 "setShardLimits" (cpu game)

unlock ∷ GameGlobal → Effect ReturnCode
unlock game = runThisEffFn0 "unlock" (cpu game)

generatePixel ∷ GameGlobal → Effect ReturnCode
generatePixel game = runThisEffFn0 "generatePixel" (cpu game)

getObjectById ∷ ∀ α. GameGlobal → Id α → Maybe α
getObjectById game id
  = toMaybe (runThisFn1 "getObjectById" game id)

notify ∷ GameGlobal → String → Effect Unit
notify game msg = runThisEffFn1 "notify" game msg

notify' ∷ GameGlobal → String → Int → Effect Unit
notify' game msg groupInterval = runThisEffFn2 "notify" game msg groupInterval
