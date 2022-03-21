module Creep where

import UPrelude
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, asks)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode ( class EncodeJson )
import Data.Argonaut.Decode ( class DecodeJson )
import Data.Newtype (class Newtype)
import Foreign.Object     as F
import Screeps.Creep      as Creep
import Screeps.RoomObject as RO
import Screeps.Data
import Data
import CorpseGrinder
import Spawn

type CreepEnv = { creep ∷ Creep
                , mem   ∷ F.Object Json }
newtype CE ε m α = CE (CreepEnv → m α)
runCE ∷ ∀ m α. CE CreepEnv m α → CreepEnv → m α
runCE (CE ce) = ce

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
instance monadReaderCE ∷ Monad m ⇒ MonadReader CreepEnv (CE ε m) where
  local f (CE m) = CE (m <<< f)
instance monadAskCE ∷ Monad m ⇒ MonadAsk CreepEnv (CE ε m) where
  ask = CE pure
instance monadTransCE ∷ MonadTrans (CE ε) where
  lift = CE <<< const

-- | special case where we are wrapped in a spwn
type Crp α = CE CreepEnv (SE SpawnEnv (CorpseGrinder Env)) α

-- | some functions lifted from Spwn
log''' ∷ LogLevel → String → Crp Unit
log''' lvl str = lift $ log'' lvl str
-- | lifted from CorpseGrinder
creepHarvest' ∷ ∀ α. RoomObject α → Crp ReturnCode
creepHarvest' obj = do
  creep ← asks (_.creep)
  lift $ lift $ creepHarvest creep obj
creepBuild' ∷ ConstructionSite → Crp ReturnCode
creepBuild' site = do
  creep ← asks (_.creep)
  lift $ lift $ creepBuild creep site
creepUpgrade' ∷ Controller → Crp ReturnCode
creepUpgrade' controller = do
  creep ← asks (_.creep)
  lift $ lift $ creepUpgrade creep controller
creepRepair' ∷ ∀ α. Structure α → Crp ReturnCode
creepRepair' struct = do
  creep ← asks (_.creep)
  lift $ lift $ creepRepair creep struct
moveCreepTo' ∷ ∀ α. TargetPosition α → Crp ReturnCode
moveCreepTo' pos = do
  creep ← asks (_.creep)
  lift $ lift $ moveCreepTo creep pos
moveCreep' ∷ Direction → Crp ReturnCode
moveCreep' dir = do
  creep ← asks (_.creep)
  lift $ lift $ moveCreep creep dir
transferResourceTo' ∷ ∀ α. RoomObject α → ResourceType → Crp ReturnCode
transferResourceTo' obj rsc = do
  creep ← asks (_.creep)
  lift $ lift $ transferResourceTo creep obj rsc

-- | getCreepMem can just use asks
getCreepMem'' ∷ Crp (F.Object Json)
getCreepMem'' = do
  mem ← asks (_.mem)
  pure mem
setCreepMem' ∷ ∀ α. (EncodeJson α) ⇒ α → Crp Unit
setCreepMem' val = do
  creep ← asks (_.creep)
  lift $ lift $ setCreepMem creep val

-- | for specific fields
getCreepMemField ∷ ∀ α. (DecodeJson α) ⇒ String → Crp (Maybe α)
getCreepMemField key = do
  mem ← asks (_.mem)
  pure $ getField' mem key
setCreepMemField' ∷ ∀ α. (EncodeJson α) ⇒ String → α → Crp Unit
setCreepMemField' key val = do
  creep ← asks (_.creep)
  lift $ lift $ setCreepMemField creep key val

-- | simplified function interface
getCreepPos ∷ Crp (TargetPosition Creep)
getCreepPos = do
  creep ← asks (_.creep)
  pure $ TargetPos $ RO.pos creep

