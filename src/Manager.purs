module Manager where

import UPrelude
import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Reader (asks)
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Structure as Structure
import Screeps.Structure.Spawn as Spawn
import Screeps.Structure.Controller as Controller
import Screeps.Store as Store
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Data
import CG
import Util (iHarvest, makeRoleArray)
import Job.Repair (manageRepairJobs)

-- | jobs are created here
manageJobs ∷ CG Env Unit
manageJobs = do
  game ← asks (_.game)
  let spawnslist      = Game.spawns      game
      creeps          = Game.creeps      game
      gcl             = Game.gcl         game
      spawn1          = F.lookup "Spawn1" spawnslist
  case spawn1 of
    -- there is no spawn yet, so just repeat this check here
    Nothing → setMemField "loopStatus" LoopStart
    Just s1 → do
      manageRepairJobs s1

-- | creeps are created here
manageCreeps ∷ CG Env Unit
manageCreeps = do
  game ← asks (_.game)
  let spawnslist      = Game.spawns      game
      creeps          = Game.creeps      game
      gcl             = Game.gcl         game
      spawn1          = F.lookup "Spawn1" spawnslist
  case spawn1 of
    -- there is no spawn yet, so just repeat this check here
    Nothing → setMemField "loopStatus" LoopStart
    Just s1 → case rcl of
      1 → do
        nMaxCreeps ← calcMaxCreeps 1 s1
        creepMem   ← getMemField "creeps"
        let numCreeps = case creepMem of
              Nothing → 1
              -- we only want to count creeps harvesting in the harvestSpot count
              Just m0 → F.size $ F.filterWithKey (iHarvest memArray) creeps
                where memArray = F.mapWithKey (makeRoleArray) m0
        -- ≤ means nMaxCreeps + 1, since we allow one over the limit
        if availableEnergy > 250 && numCreeps < nMaxCreeps then do
                    log' LogDebug "creating level 1 creep..."
                    createCreep s1 1
                  else pure unit
          where availableEnergy = Store.getUsedCapacity' spawnStore resource_energy
                spawnStore      = Spawn.store            s1
      2 → do
        nMaxCreeps ← calcMaxCreeps 2 s1
        creepMem   ← getMemField "creeps"
        let numCreeps = case creepMem of
              Nothing → 1
              -- we only want to count creeps harvesting in the harvestSpot count
              Just m0 → F.size $ F.filterWithKey (iHarvest memArray) creeps
                where memArray = F.mapWithKey (makeRoleArray) m0
        -- ≤ means nMaxCreeps + 1, since we allow one over the limit
        if availableEnergy > 250 && numCreeps < nMaxCreeps then do
                    log' LogDebug "creating level 2 creep..."
                    createCreep s1 2
                  else pure unit
          where availableEnergy = Store.getUsedCapacity' spawnStore resource_energy
                spawnStore      = Spawn.store            s1

      _ → pure unit
      where room1           = RO.room                s1
            controller1     = Room.controller        room1
            rcl             = Controller.level       controller1

-- | finds the maximum number of creeps at different levels
calcMaxCreeps ∷ Int → Spawn → CG Env Int
calcMaxCreeps 1 spawn = do
  ret ← getSpawnMem spawn "harvestSpots"
  case ret of
    Nothing → pure 0
    Just h0 → pure $ foldr (addHarvestSpots) 0 h0
calcMaxCreeps 2 spawn = do
  ret ← getSpawnMem spawn "harvestSpots"
  case ret of
    Nothing → pure 0
    Just h0 → pure $ foldr (addHarvestSpots) 0 h0
calcMaxCreeps _ _     = pure 0
addHarvestSpots ∷ HarvestSpot → Int → Int
addHarvestSpots (HarvestSpot {sourceName, nHarvs, nMaxHarvs, harvSpots}) n
  = n + nMaxHarvs

-- | basic creep creation function
createCreep ∷ Spawn → Int → CG Env Unit
createCreep spawn 1 = do
    spawnCreepWith spawn [pWork,pCarry,pMove,pMove] RoleIdle CreepPeon
createCreep spawn 2 = do
    spawnCreepWith spawn [pWork,pCarry,pMove,pMove] RoleIdle CreepPeon
createCreep _     _ = pure unit

-- | pattern match helper function
spawnCreepWith ∷ Spawn → Array BodyPartType
  → Role → CreepType → CG Env Unit
spawnCreepWith spawn parts r t = do
    let h = Structure.id spawn
    res ← spawnCreep spawn parts Nothing { typ: t, role: r, home: h, utility: 0 }
    case res of
        Nothing → log' LogWarn  "cant create creep"
        Just s0 → log' LogDebug $ s0 <> " created succesfully"
