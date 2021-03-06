module Manager where

import UPrelude
import Data.Array (foldr, length)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Structure as Structure
import Screeps.Structure.Spawn as Spawn
import Screeps.Structure.Controller as Controller
import Screeps.Store as Store
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.Const ( resource_energy, find_structures, find_sources
                     , structure_container, pWork, pMove, pCarry, pAttack)
import Data
import CorpseGrinder
import Spawn
import Util ( iHarvest, structIsType, creepIsType, makeCreepTypeArray)
--import Job.Repair (manageRepairJobs)

-- | jobs are created here
manageJobs ∷ Spwn Unit
manageJobs = do
  game ← lift $ asks (_.game)
  let spawnslist      = Game.spawns      game
      creeps          = Game.creeps      game
      gcl             = Game.gcl         game
      spawn1          = F.lookup "Spawn1" spawnslist
  case spawn1 of
    -- there is no spawn yet, so just repeat this check here
    Nothing → setMemField' "loopStatus" LoopStart
    Just s1 → do
      --manageRepairJobs s1
      pure unit

-- | creeps are created here
manageCreeps ∷ Spwn Unit
manageCreeps = do
  game   ← lift $ asks (_.game)
  spawn1 ← asks (_.spawn)
  let creeps = Game.creeps game
      gcl    = Game.gcl    game
      room1           = RO.room                spawn1
      controller1     = Room.controller        room1
      rcl             = Controller.level       controller1
      numSources      = length $ Room.find room1 find_sources
    -- TODO: functions for these that add up extensions too
      availableEnergy = Store.getUsedCapacity' spawnStore resource_energy
      energyCapacity  = Store.getCapacity'     spawnStore resource_energy
      spawnStore      = Spawn.store            spawn1
  CreepCounts {nPeon,nCollier,nHauler,nGrunt} ← calcMaxCreeps numSources
  creepMem   ← getMemField' "creeps"
  let nMaxCreeps = nPeon + nCollier + nHauler + nGrunt
      numPeons = case creepMem of
        Nothing → 0
        Just m0 → F.size $ F.filterWithKey (creepIsType CreepPeon memArray) creeps
          where memArray = F.mapWithKey (makeCreepTypeArray) m0
      numColliers = case creepMem of
        Nothing → 0
        Just m0 → F.size $ F.filterWithKey (creepIsType CreepCollier memArray) creeps
          where memArray = F.mapWithKey (makeCreepTypeArray) m0
      numHaulers = case creepMem of
        Nothing → 0
        Just m0 → F.size $ F.filterWithKey (creepIsType CreepHauler memArray) creeps
          where memArray = F.mapWithKey (makeCreepTypeArray) m0
      numGrunts = case creepMem of
        Nothing → 0
        Just m0 → F.size $ F.filterWithKey (creepIsType CreepGrunt memArray) creeps
          where memArray = F.mapWithKey (makeCreepTypeArray) m0
  -- we can use else if here since we only ever want to make one creep per tick
  if numPeons < nPeon then do
      createCreep spawn1 CreepPeon availableEnergy energyCapacity
  else if numColliers < nCollier then do
      createCreep spawn1 CreepCollier availableEnergy energyCapacity
  else if numHaulers < nHauler then do
      createCreep spawn1 CreepHauler availableEnergy energyCapacity
  else if numGrunts < nCollier then do
      createCreep spawn1 CreepGrunt availableEnergy energyCapacity
  else pure unit

-- | finds the maximum number of creeps at different levels
calcMaxCreeps ∷ Int → Spwn CreepCounts
calcMaxCreeps n = do
  ret ← getSpawnMem' "harvestSpots"
  spawn ← asks (_.spawn)
  case ret of
    Nothing → pure $ CreepCounts { nPeon: 0, nCollier: 0, nHauler: 0, nGrunt: 0 }
    Just h0 → do
      pure $ CreepCounts { nPeon: maxPeon
                         , nCollier: maxCollier
                         , nHauler: if (maxCollier > 0) then 1 else 0
                         , nGrunt: 0 }
                where maxPeon    = maxHarvs - maxCollier + n
                      maxHarvs   = foldr (addHarvestSpots) 0 h0
                      maxCollier = length containers
                      -- TODO: figure out why find_my_structures doesnt work
                      containers = Room.find' (RO.room spawn) find_structures
                                     $ structIsType structure_container
addHarvestSpots ∷ HarvestSpot → Int → Int
addHarvestSpots (HarvestSpot {sourceName, nHarvs, nMaxHarvs, harvSpots}) n
  = n + nMaxHarvs

-- | basic creep creation function
createCreep ∷ Spawn → CreepType → Int → Int → Spwn Unit
createCreep spawn CreepPeon    nrg cap =
  if cap > 350 then
    if nrg > 350 then
      spawnCreepWith spawn [pWork,pWork,pCarry,pMove,pMove,pMove] RoleIdle CreepPeon
    else pure unit
  else if cap > 250 then
    if nrg > 250 then
      spawnCreepWith spawn [pWork,pCarry,pMove,pMove] RoleIdle CreepPeon
    else pure unit
  else pure unit
createCreep spawn CreepCollier nrg cap =
  if cap > 350 then
    if nrg > 350 then
      spawnCreepWith spawn [pWork,pWork,pWork,pWork,pWork,pMove] RoleIdle CreepCollier
    else pure unit
  else if cap > 250 then
    if nrg > 250 then
      spawnCreepWith spawn [pWork,pWork,pMove,pMove] RoleIdle CreepCollier
    else pure unit
  else pure unit
createCreep spawn CreepGrunt   nrg cap =
  if cap > 350 then
    if nrg > 350 then
      spawnCreepWith spawn [pAttack,pAttack,pAttack,pMove,pMove,pMove] RoleIdle CreepGrunt
    else pure unit
  else if cap > 250 then
    if nrg > 250 then
      spawnCreepWith spawn [pAttack,pAttack,pMove,pMove] RoleIdle CreepGrunt
    else pure unit
  else pure unit
createCreep spawn CreepHauler  nrg cap =
  if cap > 350 then
    if nrg > 350 then
      spawnCreepWith spawn [pCarry,pCarry,pMove,pMove,pMove,pMove] RoleIdle CreepHauler
    else pure unit
  else if cap > 250 then
    if nrg > 250 then
      spawnCreepWith spawn [pCarry,pCarry,pMove,pMove] RoleIdle CreepHauler
    else pure unit
  else pure unit
createCreep _     _            _   cap = log'' LogWarn "i dont know how to create that creep"

-- | pattern match helper function
spawnCreepWith ∷ Spawn → Array BodyPartType
  → Role → CreepType → Spwn Unit
spawnCreepWith spawn parts r t = do
    let h = Structure.id spawn
    res ← spawnCreep' parts Nothing { typ: t, role: r, home: h, utility: 0, path:([] ∷ Path) }
    case res of
        Nothing → log'' LogWarn  "cant create creep"
        Just s0 → log'' LogDebug $ s0 <> " created succesfully"
