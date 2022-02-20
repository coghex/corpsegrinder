module Role.Harvester where
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Screeps.Const ( err_not_in_range, find_sources
                     , find_structures, resource_energy
                     , structure_spawn, structure_tower
                     , structure_extension )
import Screeps.Memory as Memory
import Screeps.Data
import Screeps.Creep as Creep
import Screeps.Room ( find, find' )
import Screeps.RoomObject ( room, storeMaybe )
import Screeps.Store as Store
import Screeps.Structure ( structureType )
import Screeps.Structure.Spawn ( store )

-- | a harvester moves between energy source and extension, spawn, or tower
preformHarvester ∷ Creep → Effect Unit
preformHarvester creep = do
  let freeCapacity = case (storeMaybe creep) of
                       Nothing → 0
                       Just s0 → Store.getFreeCapacity (Creep.store creep)
  if freeCapacity > 0 then do
    let sources = find (room creep) find_sources
    case (findNearest sources) of
      Nothing → pure unit
      Just nearestSource → do
        harv ← Creep.harvest creep nearestSource
        if harv == err_not_in_range then do
          ret ← Creep.moveTo creep (TargetObj nearestSource)
          pure unit
    else pure unit
  else do
    let targets = find' (room creep) find_structures hasFreeSpace
--    log $ "num targets: " <> (show (length targets))
    case (findNearest targets) of
      Nothing → pure unit
      Just nearestTarget → if (length targets) > 0 then do
          targ ← Creep.transfer creep nearestTarget resource_energy
          if targ == err_not_in_range then do
            ret ← Creep.moveTo creep (TargetObj nearestTarget)
            pure unit
          else pure unit
        else pure unit

hasFreeSpace ∷ ∀ a. Structure a → Boolean
hasFreeSpace structure
  =  (isStorable structure (structureType structure))
  && (spawnStoreCapacity > 0)
  where spawnStoreCapacity = case (storeMaybe structure) of
                               Nothing → 0
                               Just s0 → Store.getFreeCapacity' s0 resource_energy
isStorable ∷ ∀ a. Structure a → StructureType → Boolean
isStorable structure structure_spawn     = true
isStorable structure structure_tower     = true
isStorable structure structure_extension = true
isStorable _         _                   = false

-- TODO: write this function
findNearest ∷ ∀ a. Array a → Maybe a
findNearest arr = arr `index` 0
