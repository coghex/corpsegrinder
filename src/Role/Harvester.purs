module Role.Harvester where
import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (index, length, foldr, filter, zip, head, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Argonaut.Decode (JsonDecodeError)
import Screeps.Const ( err_not_in_range, find_sources
                     , find_structures, find_my_spawns
                     , resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Room ( find, find' )
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Store as Store
import Screeps.Source as Source
import Screeps.Structure ( structureType )
import Data (HarvestSpot(..), Spot(..))
import Util (findNearest, findNearestOpenSource
            , setNHarvs, hasFreeSpace
            , removeNHarvs)
import CG

-- | a harvester moves between energy source and extension, spawn, or tower
preformHarvester ∷ Creep → CG Env Unit
preformHarvester creep = do
  let freeCapacity = case (RO.storeMaybe creep) of
                       Nothing → 0
                       Just s0 → Store.getFreeCapacity (Creep.store creep)
  if freeCapacity > 0 then do
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
        case (findNearestOpenSource harvSs sources (RO.pos creep)) of
          Nothing → pure unit
          Just nearestSource → do
                  setCreepMem creep "target" (Source.id nearestSource)
                  case spawn of
                    Nothing → pure unit
                    Just s0 → setNHarvs harvSs (Source.id nearestSource) s0
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
  else do
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


