module Creep.Util where

import UPrelude
import Effect.Class (liftEffect)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array (head)
import Data.Newtype (class Newtype)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, getField)
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Room as Room
import Screeps.Source as Source
import Screeps.Store as Store
import Screeps.Structure as Structure
import Screeps.RoomObject as RO
import Screeps.Data
import Screeps.Const ( find_my_spawns, find_sources
                     , find_structures, resource_energy )
import Util ( findOpenSource, setNHarvs, posToSpot
            , hasFreeSpace, findNearest )
import Data
import CG

-- | sometimes we want to end up at a structure, so we check the role,
--   find the best target, set target in memory, then return it
findAndSetDestAndTarget ∷ Role → Creep → CG Env RoomPosition
findAndSetDestAndTarget RoleHarvester creep = do
  harvesting' ← getCreepMem creep "harvesting"
  harvesting  ← case harvesting' of
                  Nothing → do
                    let bool = not $ creepFull creep
                    setCreepMem creep "harvesting" bool
                    pure bool
                  Just h0 → pure h0
  if harvesting then do
    game ← asks (_.game)
    home ← getCreepMem creep "home"
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
        ret ← getSpawnMem s1 "harvestSpots"
        case ret of
          Nothing → pure []
          Just h1 → pure h1
    case (findOpenSource harvSs sources) of
      Nothing  → pure $ RO.pos creep
      Just source → do
        setCreepMem creep "target" (Source.id source)
        setCreepMem creep "dest"   $ posToSpot (RO.pos source)
        case spawn of
          Nothing → pure unit
          Just s2 → setNHarvs harvSs (Source.id source) s2
        pure $ RO.pos source
  else do
    let targets = Room.find' (RO.room creep) find_structures hasFreeSpace
    case (findNearest targets (RO.pos creep)) of
      Nothing  → pure $ RO.pos creep
      Just obj → do
        setCreepMem creep "target" (Structure.id obj)
        setCreepMem creep "dest"   $ posToSpot (RO.pos obj)
        pure $ RO.pos obj
findAndSetDestAndTarget role          creep = do
  log' LogWarn $ "role: " <> (show role) <> " has no reason to set a target"
  pure $ RO.pos creep

-- | returns true if creep is carrying nothing
creepFull ∷ Creep → Boolean
creepFull creep = case (RO.storeMaybe creep) of
  Nothing → false
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
