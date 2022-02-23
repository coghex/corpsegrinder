module Role.Harvester where
import UPrelude
import Effect (Effect)
import Effect.Console (log)
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
import Screeps.Memory as Memory
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Game as Game
import Screeps.Room ( find, find' )
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Store as Store
import Screeps.Source as Source
import Screeps.Structure ( structureType )
import Screeps.Structure.Spawn as Spawn
import Data (HarvestSpot(..), Spot(..))
import Util (findNearest, findNearestOpenSource
            , setNHarvs, hasFreeSpace
            , removeNHarvs)

-- | a harvester moves between energy source and extension, spawn, or tower
preformHarvester ∷ GameGlobal → Creep → Effect Unit
preformHarvester game creep = do
  let freeCapacity = case (RO.storeMaybe creep) of
                       Nothing → 0
                       Just s0 → Store.getFreeCapacity (Creep.store creep)
  if freeCapacity > 0 then do
    dest ← Creep.getMemory creep "target"
    case dest of
      Left  _  → do
        home ← Creep.getMemory creep "home"
        let sources = find (RO.room creep) find_sources
            -- room memory is associated with each spawn right now
            spawns  = find (RO.room creep) find_my_spawns
            -- assume one spawn per room, uses home if exists
            spawn   = case home of
                        Left  _  → head spawns
                        Right h0 → case (Game.getObjectById game h0) of
                                     Nothing → head spawns
                                     Just s0 → Just s0
        harvSs ← case spawn of
                   Nothing → pure []
                   Just s1 → do
                     ret ← Spawn.getMemory s1 "harvestSpots"
                     case ret of
                       Left  _  → pure []
                       Right h0 → pure h0
        case (findNearestOpenSource harvSs sources (RO.pos creep)) of
          Nothing → pure unit
          Just nearestSource → do
                  Creep.setMemory creep "target" (Source.id nearestSource)
                  case spawn of
                    Nothing → pure unit
                    Just s0 → setNHarvs harvSs (Source.id nearestSource) s0
      Right d0 → do
        let nearestSource' = Game.getObjectById game d0
        case nearestSource' of
          Nothing → log $ "creep " <> (Creep.name creep)
                                   <> " has lost its destination: "
                                   <> (show d0)
          Just nearestSource → do
            harv ← Creep.harvest creep nearestSource
            if harv ≡ err_not_in_range then do
              ret ← Creep.moveTo creep (TargetObj nearestSource)
              pure unit
            else pure unit
  else do
    let targets = find' (RO.room creep) find_structures hasFreeSpace
--    log $ "num targets: " <> (show (length targets))
    case (findNearest targets (RO.pos creep)) of
      Nothing → pure unit
      Just nearestTarget → if (length targets) > 0 then do
          targ ← Creep.transfer creep nearestTarget resource_energy
          if targ == err_not_in_range then do
            ret ← Creep.moveTo creep (TargetObj nearestTarget)
            pure unit
          else pure unit
        else pure unit


