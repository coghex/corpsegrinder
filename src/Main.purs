module Main where

import UPrelude
import Effect (Effect)
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe(..))
import Screeps.Memory as Memory
import Screeps.Game   as Game
import Memory (freeCreepMemory, clearMem)
import Manager (manageCreeps)
import Processor (processCreeps)
import Preformer (preformCreeps)
import Builder (buildRoom)
import Spawn (initSpawn)
import Data (LoopStatus(..))
import CG (CG, Env, LogLevel(..), getMemField, log', runCG, setMemField)

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
  log' LogInfo $ "resetting the corpsegrinder..."
  clearMem
  setMemField "loopStatus" LoopGo
  setMemField "utility"    0
  initSpawn
  manageCreeps
runCorpsegrinder (LoopError str) = log' LogError $ "Error: " <> str
runCorpsegrinder LoopNULL        = pure unit
