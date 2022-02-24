module Main where

import UPrelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Control.Monad.Reader (asks, ask)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (printJsonDecodeError, JsonDecodeError)
import Screeps.Memory as Memory
import Screeps.Game as Game
import Screeps.Creep as Creep
import Screeps.Data
import Memory
import Manager (manageCreeps)
import Processor (processCreeps)
import Preformer (preformCreeps)
import Builder (buildRoom)
import Spawn (initSpawn)
import Data
import CG
import Foreign.Object as F

checkStatus ∷ Either CGExcept Unit → Effect Unit
checkStatus (Right unit) = pure unit
checkStatus (Left  err)  = do
  log $ show err
  -- exitFailure
main ∷ Effect Unit
main = do
  m ← Memory.getMemoryGlobal
  g ← Game.getGameGlobal
  runCG corpseGrinder { memory:m, game:g }
corpseGrinder ∷ CG Env Unit
corpseGrinder = do
  memory ← asks (_.memory)
  loopStatus ← getMemField "loopStatus"
  case loopStatus of
      Nothing  → setMemField "loopStatus" LoopStart
      Just ls0 → runCorpsegrinder ls0 memory

runCorpsegrinder ∷ LoopStatus → MemoryGlobal → CG Env Unit
runCorpsegrinder LoopStart       memory = do
  {memory:memory, game:game} ← ask
  log' LogInfo "starting the corpsegrinder..."
  setMemField "loopStatus" LoopGo
  setMemField "utility"    0
  liftEffect $ initSpawn game
  liftEffect $ manageCreeps game memory
runCorpsegrinder LoopGo          memory = do
  {memory:memory, game:game} ← ask
  -- TODO: figure out how to reset the game memory
  -- check if there is a spawn, if not reset
--   if (F.size (Game.spawns game)) < 1 
--     then Memory.set memory "loopStatus" LoopReset
--     else pure unit
  let time = Game.time game
      modT = time `mod` 12
  -- the following functions will get called once every 12 ticks
  case modT of
    0 → liftEffect $ freeCreepMemory game memory
    3 → liftEffect $ manageCreeps game memory
    6 → liftEffect $ processCreeps game memory
    9 → liftEffect $ buildRoom game memory
    _ → pure unit
  liftEffect $ preformCreeps game memory
runCorpsegrinder LoopReset       memory = do
  {memory:memory, game:game} ← ask
  log' LogInfo $ "resetting the corpsegrinder..."
  liftEffect $ Memory.clear memory
  setMemField "loopStatus" LoopGo
  setMemField "utility"    0
  liftEffect $ initSpawn game
  liftEffect $ manageCreeps game memory
runCorpsegrinder (LoopError str) _      = log' LogError $ "Error: " <> str
runCorpsegrinder LoopNULL        _      = pure unit
