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

main ∷ Effect Unit
main = do
  m ← Memory.getMemoryGlobal
  g ← Game.getGameGlobal
  runCG corpseGrinder { memory:m, game:g }
corpseGrinder ∷ CG Env Unit
corpseGrinder = do
  loopStatus ← getMemField "loopStatus"
  case loopStatus of
      Nothing  → setMemField "loopStatus" LoopStart
      Just ls0 → runCorpsegrinder ls0
runCorpsegrinder ∷ LoopStatus → CG Env Unit
runCorpsegrinder LoopStart       = do
  log' LogInfo "starting the corpsegrinder..."
  setMemField "loopStatus" LoopGo
  setMemField "utility"    0
  initSpawn
  manageCreeps
runCorpsegrinder LoopGo          = do
  game ← asks (_.game)
  -- TODO: figure out how to reset the game memory
  -- check if there is a spawn, if not reset
--   if (F.size (Game.spawns game)) < 1 
--     then Memory.set memory "loopStatus" LoopReset
--     else pure unit
  let time = Game.time game
      modT = time `mod` 12
  -- the following functions will get called once every 12 ticks
  case modT of
    0 → freeCreepMemory
    3 → manageCreeps
    6 → processCreeps
    9 → buildRoom
    _ → pure unit
  preformCreeps
runCorpsegrinder LoopReset       = do
  {memory:memory, game:game} ← ask
  log' LogInfo $ "resetting the corpsegrinder..."
  liftEffect $ Memory.clear memory
  setMemField "loopStatus" LoopGo
  setMemField "utility"    0
  initSpawn
  manageCreeps
runCorpsegrinder (LoopError str) = log' LogError $ "Error: " <> str
runCorpsegrinder LoopNULL        = pure unit
