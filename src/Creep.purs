module Creep where

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
import Screeps.Creep as Creep
import Screeps.Structure as Structure
import Screeps.RoomObject as RO
import Screeps.Data
import Screeps.Const ( find_my_spawns, find_sources
                     , find_structures, resource_energy )
import Util ( findOpenSource, setNHarvs, posToSpot
            , hasFreeSpace, findNearest )
import Data
import CG

type CreepEnv = { creep ∷ Creep
                , mem   ∷ F.Object Json
                , room  ∷ Room }
newtype CE ε m α = CE (CreepEnv → m α)
runCreepEnv ∷ ∀ m α. CE CreepEnv m α → CreepEnv → m α
runCreepEnv (CE ce) = ce
derive instance newtypeCE ∷ Newtype (CE ε m α) _
instance functorCE ∷ Functor m ⇒ Functor (CE ε m) where
  map = mapCreepEnv <<< map
mapCreepEnv ∷ ∀ ε m1 m2 α β. (m1 α → m2 β) → CE ε m1 α → CE ε m2 β
mapCreepEnv f (CE m) = CE (f <<< m)
instance applicativeCE ∷ Applicative m ⇒ Applicative (CE ε m) where
  pure = CE <<< const <<< pure
instance monadCE ∷ Monad m ⇒ Monad (CE ε m)
instance bindCE ∷ Bind m ⇒ Bind (CE ε m) where
  bind (CE m) f = CE \r → m r >>= \a → case f a of
    CE f' → f' r
instance applyCE ∷ Apply m ⇒ Apply (CE ε m) where
  apply (CE f) (CE m) = CE \e → f e <*> m e
----instance monadAskCE ∷ MonadAsk CreepEnv (CE ε) where
----  ask = CE $ \e → pure e
instance monadReaderCE ∷ (Monad m) ⇒ MonadReader CreepEnv (CE ε m) where
  local f (CE m) = CE (m <<< f)
instance monadAskCE ∷ (Monad m) ⇒ MonadAsk CreepEnv (CE ε m) where
  ask = CE pure
instance monadTransCE ∷ MonadTrans (CE ε) where
  lift = CE <<< const

-- | log for this monad
log'' ∷ LogLevel → String → CE CreepEnv (CG Env) Unit
log'' lvl str = lift $ log' lvl str

-- | returns value in memory
getCreepMem' ∷ ∀ α. (DecodeJson α) ⇒ String → CE CreepEnv (CG Env) (Maybe α)
getCreepMem' field = do
  mem ← asks (_.mem)
  pure $ getField' mem field
-- | returns any boolean value in memory, with default value
getCreepMemBool ∷ String → Boolean → CE CreepEnv (CG Env) Boolean
getCreepMemBool field default = do
  mem ← asks (_.mem)
  case getField' mem field of
    Nothing → pure default
    Just v0 → pure v0
-- | returns some specific values, with corresponding defaults
getCreepMemMoving ∷ CE CreepEnv (CG Env) Boolean
getCreepMemMoving = do
  mem ← asks (_.mem)
  case (getField' mem "moving") of
    Nothing → pure true
    Just v0 → pure v0
-- | will always return a value, but will return self position if cant find
getCreepMemDest ∷ CE CreepEnv (CG Env) Spot
getCreepMemDest = do
  mem ← asks (_.mem)
  case (getField' mem "dest") of
    Nothing → do
      creep ← asks (_.creep)
      let pos = RO.pos creep
      pure $ posToSpot pos
    Just v0 → pure v0
getCreepMemTarget ∷ ∀ α. CE CreepEnv (CG Env) (Id α)
getCreepMemTarget = do
  mem ← asks (_.mem)
  case (getField' mem "target") of
    Nothing → pure $ Id "NULL"
    Just v0 → pure v0
getCreepMemHarvesting ∷ CE CreepEnv (CG Env) Boolean
getCreepMemHarvesting = do
  mem ← asks (_.mem)
  case (getField' mem "harvesting") of
    Nothing → do
      creep ← asks (_.creep)
      let freeCapacity = Store.getFreeCapacity store
          store        = Creep.store creep
      pure $ freeCapacity ≠ 0
    Just v0 → pure v0
getCreepMemHome ∷ CE CreepEnv (CG Env) (Id Spawn)
getCreepMemHome = do
  mem ← asks (_.mem)
  case (getField' mem "home") of
    Nothing → do
      creep ← asks (_.creep)
      room  ← asks (_.room)
      let spawns = Room.find room find_my_spawns
          spawn  = case head spawns of
                     Nothing → Id "NULL"
                     Just v0 → Structure.id v0
      pure spawn
    Just h0 → pure h0
getCreepMemPath ∷ CE CreepEnv (CG Env) Path
getCreepMemPath = do
  mem ← asks (_.mem)
  case (getField' mem "path") of
    Nothing → pure []
    Just v0 → pure v0


-- | finds objects in room
roomFind ∷ ∀ α. FindType α → CE CreepEnv (CG Env) (Array α)
roomFind ft = do
  creep ← asks (_.creep)
  pure $ Room.find (RO.room creep) ft
roomFind' ∷ ∀ α. FindType α → FilterFn α → CE CreepEnv (CG Env) (Array α)
roomFind' ft ff = do
  creep ← asks (_.creep)
  pure $ Room.find' (RO.room creep) ft ff

-- | functions lifted from CG
getObjectById'' ∷ ∀ α. Id α → CE CreepEnv (CG Env) (Maybe α)
getObjectById'' id = lift $ getObjectById' id
setNHarvs' ∷ Id Source → CE CreepEnv (CG Env) Unit
setNHarvs' sourceId = do
  mem    ← asks (_.mem)
  case (getField' mem "home") of
    Nothing   → pure unit
    Just home → do
      spawn  ← getObjectById'' home
      case spawn of
        Nothing → pure unit
        Just obj → do
          harvSs ← lift $ getSpawnMem obj "harvestSpots"
          case harvSs of
            Nothing → pure unit
            Just h0 → lift $ setNHarvs h0 sourceId obj
