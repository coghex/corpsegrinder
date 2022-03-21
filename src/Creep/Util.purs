module Creep.Util where
import UPrelude
import Control.Monad.Reader ( asks )
import Control.Monad.Trans.Class ( lift )
import Data.Array ( head )
import Screeps.RoomObject as RO
import Screeps.Game       as Game
import Screeps.Structure  as Structure
import Screeps.Store      as Store
import Screeps.Source     as Source
import Screeps.Room       as Room
import Screeps.Const ( resource_energy, find_my_spawns
                     , find_sources, find_structures )
import Screeps.Data
import Data
import Util ( findOpenSource, posToSpot, setNHarvs
            , hasFreeSpace, findNearest )
import Spawn
import Creep

-- | sometimes we want to end up at a structure, so we check the role,
--   find the best target, set target in memory, then return it
findAndSetDestAndTarget ∷ Role → Crp RoomPosition
findAndSetDestAndTarget RoleHarvester = do
  creep ← asks (_.creep)
  harvesting' ← getCreepMemField "harvesting"
  harvesting  ← case harvesting' of
                  Nothing → do
                    cFull ← creepFull
                    let bool = not $ cFull
                    setCreepMemField' "harvesting" bool
                    pure bool
                  Just h0 → pure h0
  if harvesting then do
    game ← lift $ lift $ asks (_.game)
    home ← getCreepMemField "home"
    let spawn = case home of
                  Nothing → head spawns
                  Just h0 → case (Game.getObjectById game h0) of
                    Nothing → head spawns
                    Just s0 → Just s0
        spawns  = Room.find (RO.room creep) find_my_spawns
        sources = Room.find (RO.room creep) find_sources
    harvSs ← case spawn of
      Nothing → pure []
      Just s1 → do
        ret ← lift $ getSpawnMem' "harvestSpots"
        case ret of
          Nothing → pure []
          Just h1 → pure h1
    case (findOpenSource harvSs sources) of
      Nothing  → pure $ RO.pos creep
      Just source → do
        targId ← getCreepMemField "target"
        let targId' = targId ∷ Maybe (Id Source)
        case targId of
          Nothing → do
            setCreepMemField' "target" (Source.id source)
            setCreepMemField' "dest"   $ posToSpot (RO.pos source)
            case spawn of
              Nothing → pure unit
              Just s2 → lift $ lift $ setNHarvs harvSs (Source.id source) s2
            pure $ RO.pos source
          Just tid → do
            setCreepMemField' "target" (Source.id source)
            setCreepMemField' "dest"   $ posToSpot (RO.pos source)
            pure $ RO.pos source

  else do
    let targets = Room.find' (RO.room creep) find_structures hasFreeSpace
    case (findNearest targets (RO.pos creep)) of
      Nothing  → pure $ RO.pos creep
      Just obj → do
        setCreepMemField' "target" (Structure.id obj)
        setCreepMemField' "dest"   $ posToSpot (RO.pos obj)
        pure $ RO.pos obj
findAndSetDestAndTarget role          = do
  log''' LogWarn $ "role: " <> (show role) <> " has no reason to set a target"
  creep ← asks (_.creep)
  pure $ RO.pos creep

-- | returns true if creep is carrying nothing
creepFull ∷ Crp Boolean
creepFull = do
  creep ← asks (_.creep)
  pure $ case (RO.storeMaybe creep) of
    Nothing → false
    Just s0 → (Store.getFreeCapacity s0) ≡ 0
-- | returns true if creep is carrying nothing
creepEmpty ∷ Crp Boolean
creepEmpty = do
  creep ← asks (_.creep)
  pure $ case (RO.storeMaybe creep) of
    Nothing → false
    Just s0 → (Store.getUsedCapacity s0) ≡ 0

-- | returns true if creep has any enery
creepHasEnergy ∷ Crp Boolean
creepHasEnergy = do
  creep ← asks (_.creep)
  pure $ case (RO.storeMaybe creep) of
    Nothing → false
    Just s0 → Store.getUsedCapacity' s0 resource_energy ≠ 0
-- | returns the energy currently held by a creep
creepSpaceForEnergy ∷ Crp Int
creepSpaceForEnergy = do
  creep ← asks (_.creep)
  pure $ case (RO.storeMaybe creep) of
    Nothing → 0
    Just s0 → Store.getFreeCapacity' s0 resource_energy

-- | returns current position of creep
creepPos ∷ Crp RoomPosition
creepPos = do
  creep ← asks (_.creep)
  pure $ RO.pos creep
