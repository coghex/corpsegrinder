module CG where

import UPrelude
import Screeps.Data
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Control.Lazy (class Lazy)
import Control.Monad.Reader ( asks )
import Data.DateTime as DT
import Data.Date as Date
import Data.Time as Time
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson, getField
                            , printJsonDecodeError
                            , JsonDecodeError)
import Screeps.Memory as Memory
import Screeps.Game   as Game
import Screeps.Room   as Room
import Screeps.RoomObject as RO
import Screeps.Creep  as Creep
import Screeps.Const
import Screeps.Structure.Spawn as Spawn
import Screeps.FFI (unsafeDeleteFieldEff)
import Foreign.Object as F
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Category (identity)
import Data.Newtype (class Newtype)

-- read only environment
type Env = { memory ∷ MemoryGlobal, game ∷ GameGlobal }

-- continuation monad
newtype CG ε α = CG (Env → Effect α)
runCG ∷ ∀ α. CG Env α → Env → Effect α
runCG (CG cg) = cg

-- Exception data
data CGExcept = CGSuccess | CGError | CGNULL
instance showCGExcept ∷ Show CGExcept where
  show CGSuccess = "success"
  show CGError   = "error"
  show CGNULL    = "NULL"

-- Log data
data LogLevel = LogDebug | LogInfo | LogWarn | LogError | LogNULL
derive instance eqLogLevel   ∷ Eq   LogLevel
derive instance ordLogLevel  ∷ Ord  LogLevel
instance showLogLevel ∷ Show LogLevel where
  show LogDebug = "Debug"
  show LogInfo  = "Info"
  show LogWarn  = "Warn"
  show LogError = "Error"
  show LogNULL  = "NULL"
type LogStr = { level ∷ LogLevel
              , time  ∷ DT.DateTime
              , msg   ∷ String }

-- logger class
class MonadLog μ where
  logIO ∷ LogStr → μ Unit

-- i dont know why i do this part
derive instance newtypeCG ∷ Newtype (CG ε α) _

-- derive some instances to make it a monad
derive instance functorCG ∷ Functor (CG ε)
instance applicativeCG ∷ Applicative (CG ε) where
  pure a = CG $ \_ → pure $ a
instance monadCG ∷ Monad (CG ε)
instance bindCG ∷ Bind (CG ε) where
  bind (CG m) f = CG \r → m r >>= \a → case f a of
    CG f' → f' r <#> \res → res
instance applyCG ∷ Apply (CG ε) where
  apply (CG f) (CG m) = CG \r → f r >>= \f' → m r <#> \a'' → f' a''
-- instance monadTransCG ∷ MonadTrans (CG Env) where
--   lift m = CG \_ → m >>= \a → pure $ CGResult a
-- instance lazyCG ∷ Lazy (CG Env m α) where
--   defer f = CG \r → case f unit of CG f' → f' r
instance monadEffectCG ∷ MonadEffect (CG ε) where
  liftEffect m = CG \_ → m
-- reader
instance monadAskCG    ∷ MonadAsk    Env   (CG ε) where
  ask       = CG $ \e → pure e
  -- TODO: fill in this placeholder
instance monadReaderCG ∷ MonadReader Env   (CG ε) where
  local f m = m

instance monadLogCG ∷ MonadLog (CG ε) where
  logIO {level:lvl,time:t,msg:m}
    = liftEffect $ log $ "[" <> (format t) <> "]: " <> (show lvl) <> ": " <> m

format ∷ DT.DateTime → String
format dt@(DT.DateTime d t) = (show $ fromEnum day)   <> "/"
                           <> show month              <> "/"
                           <> (show $ fromEnum year)  <> ": "
                           <> (show $ fromEnum hour)  <> ":"
                           <> (show $ fromEnum min)   <> ":"
                           <> (show $ fromEnum sec)
  where day   = Date.day    d
        month = Date.month  d
        year  = Date.year   d
        hour  = Time.hour   t
        min   = Time.minute t
        sec   = Time.second t

-- base logging function
log' ∷ LogLevel → String → CG Env Unit
log' lvl str = liftEffect nowDateTime >>= logIO <<< { level: lvl, time: _, msg: str }

-- this is a simple translation so that we can automatically ignore errors
getField' ∷ ∀ α. DecodeJson α ⇒ F.Object Json → String → Maybe α
getField' obj key = case getField obj key of
                      Left  _ → Nothing
                      Right v → Just v

-- gets a memory field, decodes json into maybe structure
getMemField ∷ ∀ α. (DecodeJson α) ⇒ String → CG Env (Maybe α)
getMemField field = do
  mem ← asks (_.memory)
  ret ← liftEffect $ Memory.get mem field
  case ret of
    Left err → do
      log' LogError $ printJsonDecodeError err
      pure Nothing
    Right v0 → pure v0
-- sets a memory field
setMemField ∷ ∀ α. (EncodeJson α) ⇒ String → α → CG Env Unit
setMemField field val = do
  mem ← asks (_.memory)
  liftEffect $ Memory.set mem field val

-- gets and sets memory for spawns
getSpawnMem ∷ ∀ α. (DecodeJson α) ⇒ Spawn → String → CG Env (Maybe α)
getSpawnMem spawn field = do
  ret ← liftEffect $ Spawn.getMemory spawn field
  case ret of
    Left err → do
      log' LogError $ printJsonDecodeError err
      pure Nothing
    Right v0 → pure v0
setSpawnMem ∷ ∀ α. (EncodeJson α) ⇒ Spawn → String → α → CG Env Unit
setSpawnMem spawn field val = liftEffect $ Spawn.setMemory spawn field val

-- spawns a creep with memory, returns a name
spawnCreep ∷ ∀ α. (EncodeJson α) ⇒ Spawn → Array BodyPartType
  → Maybe String → α → CG Env (Maybe String)
spawnCreep spawn parts name' mem = do
  ret ← liftEffect $ Spawn.spawnCreep' spawn parts name' mem
  case ret of
    Left err → do
      log' LogError $ show err
      pure Nothing
    Right r0 → pure $ Just r0

-- gets and sets memory for creeps, all at once, or in fields
getAllCreepMem ∷ ∀ α. (DecodeJson α) ⇒ Creep → CG Env (Maybe α)
getAllCreepMem creep = do
  ret ← liftEffect $ Creep.memory creep
  case ret of
    Left err → do
      log' LogError $ printJsonDecodeError err
      pure Nothing
    Right v0 → pure v0
getCreepMem ∷ ∀ α. (DecodeJson α) ⇒ Creep → String → CG Env (Maybe α)
getCreepMem creep field = do
  ret ← liftEffect $ Creep.getMemory creep field
  case ret of
    Left err → do
      log' LogError $ printJsonDecodeError err
      pure Nothing
    Right v0 → pure v0
setCreepMem ∷ ∀ α. (EncodeJson α) ⇒ Creep → String → α → CG Env Unit
setCreepMem creep field val = liftEffect $ Creep.setMemory creep field val

-- | clears all data, used for reset
clearMem ∷ CG Env Unit
clearMem = do
  memory ← asks (_.memory)
  liftEffect $ Memory.clear memory

-- | clears just the data for a creep
freeCreepMem ∷ String → CG Env Unit
freeCreepMem n = do
  memory ← asks (_.memory)
  liftEffect $ Memory.freeCreep memory n

-- | clears a single field of creep data, at a ffi level
freeCreepMemField ∷ Creep → String → CG Env Unit
freeCreepMemField creep str = liftEffect $ unsafeDeleteFieldEff str creep

-- | some other creep functions
creepHarvest ∷ ∀ α. Creep → RoomObject α → CG Env ReturnCode
creepHarvest creep obj = liftEffect $ Creep.harvest creep obj
transferResourceTo ∷ ∀ α. Creep → RoomObject α → ResourceType → CG Env ReturnCode
transferResourceTo creep obj rsc = liftEffect $ Creep.transfer creep obj rsc
moveCreepTo ∷ ∀ α. Creep → TargetPosition α → CG Env ReturnCode
moveCreepTo creep pos = liftEffect $ Creep.moveTo creep pos
creepBuild ∷ Creep → ConstructionSite → CG Env ReturnCode
creepBuild creep site = liftEffect $ Creep.build creep site
creepUpgrade ∷ Creep → Controller → CG Env ReturnCode
creepUpgrade creep controller = liftEffect
  $ Creep.upgradeController creep controller
creepRepair ∷ ∀ α. Creep → Structure α → CG Env ReturnCode
creepRepair creep struct = liftEffect
  $ Creep.repair creep struct

-- room functions
createConstructionSite ∷ ∀ α. Room → TargetPosition α → StructureType → CG Env Unit
createConstructionSite room pos stype = do
  ret ← liftEffect $ Room.createConstructionSite room pos structure_container
  if ret ≡ ok then
    log' LogDebug $ "creating structure: " <> (show structure_container)
  else log' LogDebug $ "createConstructionSite error: " <> (show ret)


-- game functions
getObjectById' ∷ ∀ α. Id α → CG Env (Maybe α)
getObjectById' id = do
  game ← asks (_.game)
  pure $ Game.getObjectById game id
