module Creep.Peon where
-- ^ peons are the lowest level of creep

import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (head, uncons, length)
import Data.Maybe (Maybe(..))
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Room ( find, find' )
import Screeps.RoomObject as RO
import Screeps.Source as Source
import Screeps.Store as Store
import Screeps.Const (find_sources, find_my_spawns
                     , find_structures, resource_energy
                     , err_not_in_range)
import Util (findNearest, findNearestOpenSource
            , setNHarvs, hasFreeSpace
            , removeNHarvs, findOpenSource)
import CG

-- | checks if we were already going somewhere and if not
--   finds a new source.
getEnergy ∷ Creep → CG Env Unit
getEnergy creep = do
    dest ← getCreepMem creep "target"
    case dest of
      Nothing → do
        home ← getCreepMem creep "home"
        game ← asks (_.game)
        let sources = find (RO.room creep) find_sources
            -- room memory is associated with each spawn right now
            spawns  = find (RO.room creep) find_my_spawns
            -- assume one spawn per room, uses home if exists
            spawn   = case home of
                        Nothing → head spawns
                        Just h0 → case (Game.getObjectById game h0) of
                                     Nothing → head spawns
                                     Just s0 → Just s0
        harvSs ← case spawn of
                   Nothing → pure []
                   Just s1 → do
                     ret ← getSpawnMem s1 "harvestSpots"
                     case ret of
                       Nothing → pure []
                       Just h0 → pure h0
        -- set to a random target
        rand ← randomNumber 0
        case (findOpenSource harvSs sources rand) of
          Nothing  → pure unit
          Just sid → do
                       setCreepMem creep "target" (Source.id sid)
                       case spawn of
                         Nothing → pure unit
                         Just s0 → setNHarvs harvSs (Source.id sid) s0
--        case (findNearestOpenSource harvSs sources (RO.pos creep)) of
--          Nothing → pure unit
--          Just nearestSource → do
--                  setCreepMem creep "target" (Source.id nearestSource)
--                  case spawn of
--                    Nothing → pure unit
--                    Just s0 → setNHarvs harvSs (Source.id nearestSource) s0
      Just d0 → do
        game ← asks (_.game)
        let nearestSource' = Game.getObjectById game d0
        case nearestSource' of
          Nothing → log' LogWarn $ "creep " <> (Creep.name creep)
                                            <> " has lost its destination: "
                                            <> (show d0)
          Just nearestSource → do
            harv ← creepHarvest creep nearestSource
            if harv ≡ err_not_in_range then do
              ret ← moveCreepTo creep (TargetObj nearestSource)
              pure unit
            else pure unit
storeEnergy ∷ Creep → CG Env Unit
storeEnergy creep = do
    let targets = find' (RO.room creep) find_structures hasFreeSpace
--    log $ "num targets: " <> (show (length targets))
    case (findNearest targets (RO.pos creep)) of
      Nothing → pure unit
      Just nearestTarget → if (length targets) > 0 then do
          targ ← transferResourceTo creep nearestTarget resource_energy
          if targ == err_not_in_range then do
            ret ← moveCreepTo creep (TargetObj nearestTarget)
            pure unit
          else pure unit
        else pure unit
-- | returns true if creep is carrying nothing
creepFull ∷ Creep → Boolean
creepFull creep = case (RO.storeMaybe creep) of
  Nothing → false
  -- i dont know why i had this line like this, but it could be important
  --Just _  → (Store.getFreeCapacity (Creep.store creep)) > 0
  Just s0 → (Store.getFreeCapacity s0) ≡ 0
-- | returns true if creep is carrying nothing
creepEmpty ∷ Creep → Boolean
creepEmpty creep = case (RO.storeMaybe creep) of
  Nothing → false
  Just s0 → (Store.getUsedCapacity s0) ≡ 0

-- | returns true if creep has any enery
creepHasEnergy ∷ Creep → Boolean
creepHasEnergy creep = case (RO.storeMaybe creep) of
  Nothing → false
  Just s0 → Store.getUsedCapacity' s0 resource_energy ≠ 0
-- | returns the energy currently held by a creep
creepSpaceForEnergy ∷ Creep → Int
creepSpaceForEnergy creep = case (RO.storeMaybe creep) of
  Nothing → 0
  Just s0 → Store.getFreeCapacity' s0 resource_energy
