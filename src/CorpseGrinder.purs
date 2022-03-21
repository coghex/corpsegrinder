module CorpseGrinder where

import UPrelude
import Effect ( Effect )
import Effect.Class ( class MonadEffect, liftEffect )
import Effect.Console ( log )
import Effect.Now ( nowDateTime )
import Control.Monad.Reader ( asks )
import Control.Monad.Reader.Class ( class MonadAsk
                                  , class MonadReader )
import Data.Newtype ( class Newtype )
import Data.DateTime as DT
import Data.Date     as Date
import Data.Time     as Time
import Data.Enum ( fromEnum )
import Data.Argonaut.Core ( Json )
import Data.Argonaut.Encode ( class EncodeJson )
import Data.Argonaut.Decode ( class DecodeJson
                            , getField
                            , printJsonDecodeError
                            , JsonDecodeError )
import Foreign.Object          as F
import Screeps.FFI             as FFI
import Screeps.Memory          as Memory
import Screeps.Game            as Game
import Screeps.Creep           as Creep
import Screeps.Room            as Room
import Screeps.RoomObject      as RO
import Screeps.Structure.Spawn as Spawn
import Screeps.Data
import Screeps.Const ( ok, structure_container )
import Data

-- | read only environment
type Env = { memory ∷ MemoryGlobal
           , game   ∷ GameGlobal
           , time   ∷ Int }
-- | continuation monad
newtype CorpseGrinder ε α = CorpseGrinder (Env → Effect α)
runCG ∷ ∀ α.  CorpseGrinder Env α → Env → Effect α
runCG (CorpseGrinder cg) = cg

type LogStr = { level ∷ LogLevel
              , time  ∷ DT.DateTime
              , msg   ∷ String }
-- logger class
class MonadLog μ where
  logIO ∷ LogStr → μ Unit
-- monadic instances
derive instance newtypeCorpseGrinder ∷ Newtype         (CorpseGrinder ε α) _
derive instance functorCorpseGrinder ∷ Functor         (CorpseGrinder ε)
instance applicativeCorpseGrinder    ∷ Applicative     (CorpseGrinder ε) where
  pure a = CorpseGrinder $ \_ → pure $ a
instance monadCorpseGrinder          ∷ Monad           (CorpseGrinder ε)
instance bindCorpseGrinder           ∷ Bind            (CorpseGrinder ε) where
  bind (CorpseGrinder m) f = CorpseGrinder \r → m r >>= \a → case f a of
    CorpseGrinder f' → f' r <#> \res → res
instance applyCorpseGrinder          ∷ Apply           (CorpseGrinder ε) where
  apply (CorpseGrinder f) (CorpseGrinder m) =
    CorpseGrinder \r → f r >>= \f' → m r <#> \a'' → f' a''
instance monadEffectCorpseGrinder    ∷ MonadEffect     (CorpseGrinder ε) where
  liftEffect m = CorpseGrinder \_ → m
instance monadAskCorpseGrinder       ∷ MonadAsk    Env (CorpseGrinder ε) where
  ask          = CorpseGrinder $ \e → pure e
instance monadReaderCorpseGrinder    ∷ MonadReader Env (CorpseGrinder ε) where
  local f m = m
instance monadLogCorpseGrinder       ∷ MonadLog        (CorpseGrinder ε) where
  logIO { level:lvl,time:t,msg:m }
    = liftEffect $ log $ "[" <> (format t) <> "]: " <> (show lvl) <> ": " <> m
format ∷ DT.DateTime → String
format dt@(DT.DateTime d t) = (show $ fromEnum day)   <> "/"
                           <> show month              <> "/"
                           <> (show $ fromEnum year)  <> ": "
                           <> hour'                   <> ":"
                           <> min'                    <> ":"
                           <> sec'
  where day   = Date.day    d
        month = Date.month  d
        year  = Date.year   d
        hour  = Time.hour   t
        min   = Time.minute t
        sec   = Time.second t
        hour' = if (fromEnum hour) < 10
                  then "0" <> (show (fromEnum hour))
                  else show $ fromEnum hour
        min'  = if (fromEnum min) < 10
                  then "0" <> (show (fromEnum min))
                  else show $ fromEnum min
        sec'  = if (fromEnum sec) < 10
                  then "0" <> (show (fromEnum sec))
                  else show $ fromEnum sec

-- | base logging function
log' ∷ LogLevel → String → CorpseGrinder Env Unit
log' lvl str = liftEffect nowDateTime >>= logIO <<< { level:lvl,time:_,msg:str }

-- | clears the console using html js hack
consoleClear ∷ CorpseGrinder Env Unit
consoleClear = liftEffect FFI.consoleClear

-- | returns list of spawns
getSpawns ∷ CorpseGrinder Env (F.Object Spawn)
getSpawns = do
  g ← asks (_.game)
  pure $ Game.spawns g

getCreeps ∷ CorpseGrinder Env (F.Object Creep)
getCreeps = do
  g ← asks (_.game)
  pure $ Game.creeps g

-- | returns time
getTime ∷ CorpseGrinder Env Int
getTime = asks (_.time)

-- | simple wrapper to treat either as maybe
getField' ∷ ∀ α. DecodeJson α ⇒ F.Object Json → String → Maybe α
getField' obj key = case getField obj key of
                      Left  _ → Nothing
                      Right v → Just v

-- | gets memory field, decodes json, processes error, then returns maybe
getMemField ∷ ∀ α. (DecodeJson α) ⇒ String → CorpseGrinder Env (Maybe α)
getMemField field = do
  mem ← asks (_.memory)
  ret ← liftEffect $ Memory.get mem field
  case ret of
    Left err → do
      log' LogError $ "getMemField: " <> printJsonDecodeError err
      pure Nothing
    Right v0 → pure v0

-- | set memory field
setMemField ∷ ∀ α. (EncodeJson α) ⇒ String → α → CorpseGrinder Env Unit
setMemField field val = do
  mem ← asks (_.memory)
  liftEffect $ Memory.set mem field val

-- | game functions that can be expressed easier
getObjectById' ∷ ∀ α. Id α → CorpseGrinder Env (Maybe α)
getObjectById' id = do
  game ← asks (_.game)
  pure $ Game.getObjectById game id

-- | returns false if there was an error
createConstructionSite ∷ ∀ α. Room → TargetPosition α
                              → StructureType → CorpseGrinder Env Boolean
createConstructionSite room pos stype = do
  ret ← liftEffect $ Room.createConstructionSite room pos structure_container
  if ret ≡ ok then do
    --log' LogDebug $ "creating structure: " <> (show structure_container)
    pure true
  else do
    --log' LogDebug $ "createConstructionSite error: " <> (show ret)
    pure false

-- | simple ffi
creepHarvest ∷ ∀ α. Creep → RoomObject α → CorpseGrinder Env ReturnCode
creepHarvest creep obj = liftEffect $ Creep.harvest creep obj
creepBuild ∷ Creep → ConstructionSite → CorpseGrinder Env ReturnCode
creepBuild creep site = liftEffect $ Creep.build creep site
creepUpgrade ∷ Creep → Controller → CorpseGrinder Env ReturnCode
creepUpgrade creep controller = liftEffect
  $ Creep.upgradeController creep controller
creepRepair ∷ ∀ α. Creep → Structure α → CorpseGrinder Env ReturnCode
creepRepair creep struct = liftEffect
  $ Creep.repair creep struct
moveCreepTo ∷ ∀ α. Creep → TargetPosition α → CorpseGrinder Env ReturnCode
moveCreepTo creep pos = liftEffect $ Creep.moveTo creep pos
moveCreep ∷ Creep → Direction → CorpseGrinder Env ReturnCode
moveCreep creep dir = liftEffect $ Creep.move creep dir
transferResourceTo ∷ ∀ α. Creep → RoomObject α
  → ResourceType → CorpseGrinder Env ReturnCode
transferResourceTo creep obj rsc = liftEffect $ Creep.transfer creep obj rsc

-- | clears just the data for a creep
freeCreepMem ∷ String → CorpseGrinder Env Unit
freeCreepMem n = do
  memory ← asks (_.memory)
  liftEffect $ Memory.freeCreep memory n
-- | sets memory for a spawn
setSpawnMem ∷ ∀ α. (EncodeJson α)
  ⇒ Spawn → String → α → CorpseGrinder Env Unit
setSpawnMem spawn field val = liftEffect $ Spawn.setMemory spawn field val
-- | gets memory for a spawn
getSpawnMem ∷ ∀ α. (DecodeJson α)
  ⇒ Spawn → String → CorpseGrinder Env (Maybe α)
getSpawnMem spawn field = do
  ret ← liftEffect $ Spawn.getMemory spawn field
  case ret of
    Left err → do
      log' LogError $ "getSpawnMem: " <> printJsonDecodeError err
      pure Nothing
    Right v0 → pure v0

-- spawns a creep with memory, returns a name
spawnCreep ∷ ∀ α. (EncodeJson α) ⇒ Spawn → Array BodyPartType
  → Maybe String → α → CorpseGrinder Env (Maybe String)
spawnCreep spawn parts name' mem = do
  ret ← liftEffect $ Spawn.spawnCreep' spawn parts name' mem
  case ret of
    Left err → do
      log' LogError $ "spawnCreep: " <> show err
      pure Nothing
    Right r0 → pure $ Just r0

-- | gets memory for a creep
getCreepMem ∷ ∀ α. (DecodeJson α)
  ⇒ Creep → CorpseGrinder Env (Maybe α)
getCreepMem creep = do
  ret ← liftEffect $ Creep.memory creep
  case ret of
    Left err → do
      log' LogError $ "getCreepMem: " <> printJsonDecodeError err
      pure Nothing
    Right v0 → pure v0
setCreepMem ∷ ∀ α. (EncodeJson α)
  ⇒ Creep → α → CorpseGrinder Env Unit
setCreepMem creep val = liftEffect $ Creep.setAllMem creep val

setCreepMemField ∷ ∀ α. (EncodeJson α)
  ⇒ Creep → String → α → CorpseGrinder Env Unit
setCreepMemField creep key val = liftEffect $ Creep.setMemory creep key val

-- these are not even close to really being random
-- TODO: Math.random functions from js should be in FFI
-- | random number from a seed
randomNumber ∷ Int → CorpseGrinder Env Int
randomNumber seed = do
  game ← asks (_.game)
  let time = Game.time game
  pure $ time `mod` seed
-- | random boolean from seed
randomBoolean ∷ Int → CorpseGrinder Env Boolean
randomBoolean seed = do
  game ← asks (_.game)
  let time = Game.time game
      val  = (time `mod` seed) `mod` 2
  pure $ val > 0
