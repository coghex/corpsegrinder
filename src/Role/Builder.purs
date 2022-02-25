module Role.Builder where
import UPrelude
import Control.Monad.Reader (asks)
import Data.Array (index, length, head)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, getField)
import Screeps.Const ( err_not_in_range, find_sources, find_my_spawns
                     , find_construction_sites, resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Room ( find, find' )
import Screeps.RoomObject as RO
import Screeps.Game as Game
import Screeps.Store as Store
import Screeps.Source as Source
import Screeps.Structure ( structureType )
import Foreign.Object as F
import Util (findNearest, findNearestOpenSource, setNHarvs, removeNHarvs)
import CG

-- | a builder moves between mining for energy and building construction sites
preformBuilder ∷ Creep → CG Env Unit
preformBuilder creep = do
  mem ← getAllCreepMem creep
  case mem of
    Nothing → pure unit
    Just d0 → preformBuilderF creep d0
preformBuilderF ∷ Creep → F.Object Json → CG Env Unit
preformBuilderF creep mem = do
  case (getField mem "building") of
    -- this means its a new builder, so set the memory
    -- accordingly first, then continue
    Left  _        → do
      setCreepMem creep "building" false
      preformBuilderFF creep false
    -- if we are already a builder this will be set already
    Right building → preformBuilderFF creep building

preformBuilderFF ∷ Creep → Boolean → CG Env Unit
preformBuilderFF creep building = if building then do
    let energy = case (RO.storeMaybe creep) of
                   Nothing → 0
                   Just s0 → Store.getUsedCapacity' s0 resource_energy
        freeCapacity = case (RO.storeMaybe creep) of
                         Nothing → 0
                         Just c0 → Store.getFreeCapacity c0
    if energy <= 0 then do
      setCreepMem creep "building" false
    else if freeCapacity == 0 then do
      setCreepMem creep "building" true
    else pure unit
    let targets = find (RO.room creep) find_construction_sites
    case (findNearest targets (RO.pos creep)) of
      Nothing → pure unit
      Just nearestTarget → if (length targets > 0) then do
        res ← creepBuild creep nearestTarget
        if (res == err_not_in_range) then do
          ret ← moveCreepTo creep (TargetObj nearestTarget)
          pure unit
        else pure unit
   else pure unit
  else do
    dest ← getCreepMem creep "target"
    case dest of
      Nothing → do
        game ← asks (_.game)
        home ← getCreepMem creep "home"
        let sources = find (RO.room creep) find_sources
            spawns  = find (RO.room creep) find_my_spawns
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
