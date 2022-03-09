module Creep.Peon where
-- ^ peons are the lowest level of creep

import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (head, uncons, length)
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.Source as Source
import Screeps.Const (find_sources, find_my_spawns
                     , find_structures, resource_energy
                     , err_not_in_range, ok)
import Util (findNearest, findNearestOpenSource
            , setNHarvs, hasFreeSpace
            , removeNHarvs, findOpenSource
            , posToSpot, spotToPos, posEqSpot)
import Creep.Util (creepFull, findAndSetDestAndTarget)
import Data
import CG

-- | peons move to a room position
peonMove ∷ Creep → RoomPosition → CG Env Unit
peonMove creep dest = do
  pathMem ← getCreepMem creep "path"
  path0 ← case pathMem of
    Nothing → do
      let path     = Room.findPath (RO.room creep) (RO.pos creep) dest
          destSpot = posToSpot dest
      setCreepMem creep "path"   path
      setCreepMem creep "dest"   destSpot
      pure path
    Just [] → do
      let path     = Room.findPath (RO.room creep) (RO.pos creep) dest
          destSpot = posToSpot dest
      setCreepMem creep "path"   path
      setCreepMem creep "dest"   destSpot
      pure path
    Just p0 → pure p0
  peonMoveAlongPath creep path0
peonMoveAlongPath ∷ Creep → Path → CG Env Unit
peonMoveAlongPath creep []   = do
  dest' ← getCreepMem creep "dest"
  case dest' of
    Nothing → log' LogWarn "creep has no dest"
    Just d0 → if (RO.pos creep) `posEqSpot` d0 then
                setCreepMem creep "moving" false
              else
                setCreepMem creep "path" path
                where path = Room.findPath (RO.room creep) (RO.pos creep) d0'
                      name = Room.name (RO.room creep)
                      d0'  = spotToPos (Room.name (RO.room creep)) d0
peonMoveAlongPath creep path = case uncons path of
    Nothing               → log' LogError "this error is not possible"
    Just {head:h,tail:[]} → do
      setCreepMem creep "moving"     false
      setCreepMem creep "path"       ([] ∷ Path)
    Just {head:h,tail:t}  → do
      ret ← moveCreep creep h.direction
      let cn = Creep.name creep
      if ret ≠ ok then do
        dest ← getCreepMem creep "dest"
        case dest of
          Nothing → log' LogWarn "creep has path but no dest"
          Just d0 → if (RO.pos creep) `posEqSpot` d0 then
              setCreepMem creep "moving" false
            else pure unit
      else setCreepMem creep "path" t

-- | all peons will need to harvest to get their energy
peonHarvest ∷ Creep → CG Env Unit
peonHarvest creep = do
  target ← getCreepMem creep "target"
  case target of
    Nothing → log' LogError "peon lost its harvest target"
    Just t0 → do
      source' ← getObjectById' t0
      case source' of
        Nothing → log' LogError "source destination no longer exists"
        Just s0 → do
          harv ← creepHarvest creep s0
          if harv ≠ ok then do
--            log' LogWarn $ "peon was unable to harvest: " <> (show harv)
            setCreepMem creep "moving"     true
          else if creepFull creep then do
            setCreepMem creep "harvesting" false
            setCreepMem creep "moving"     true
            dest ← findAndSetDestAndTarget RoleHarvester creep
            peonMove creep dest
          else pure unit

-- | checks if we were already going somewhere and if not
--   finds a new source.
getEnergy ∷ Creep → CG Env Unit
getEnergy creep = do
    dest ← getCreepMem creep "target"
    case dest of
      Nothing → do
        home ← getCreepMem creep "home"
        game ← asks (_.game)
        let sources = Room.find (RO.room creep) find_sources
            -- room memory is associated with each spawn right now
            spawns  = Room.find (RO.room creep) find_my_spawns
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
        case (findOpenSource harvSs sources) of
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
    let targets = Room.find' (RO.room creep) find_structures hasFreeSpace
    case (findNearest targets (RO.pos creep)) of
      Nothing → pure unit
      Just nearestTarget → if (length targets) > 0 then do
          targ ← transferResourceTo creep nearestTarget resource_energy
          if targ == err_not_in_range then do
            ret ← moveCreepTo creep (TargetObj nearestTarget)
            pure unit
          else pure unit
        else pure unit

-- | trys to dropoff energy
peonDeposit ∷ Creep → CG Env Unit
peonDeposit creep = do
  target' ← getCreepMem creep "target"
  case target' of
    Nothing → log' LogWarn "creep has lost deposit target"
    Just tid → do
      obj ← getObjectById' tid
      case obj of
        Nothing   → log' LogWarn "creep has lost its deposit target id"
        Just tObj → do
          targ   ← transferResourceTo creep tObj resource_energy
          if targ ≠ ok then do
            log' LogError $ "target error: " <> (show targ)
            setCreepMem creep "moving" true
          else do
            setCreepMem creep "harvesting" true
            dest ← findAndSetDestAndTarget RoleHarvester creep
            peonMove creep dest
