module Main where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (printJsonDecodeError, JsonDecodeError)
import Screeps.Memory as Memory
import Screeps.Game as Game
import Screeps.Creep as Creep
import Screeps.Data
import Manager (manageCreeps)
import Processor (processCreeps)
import Preformer (preformCreeps)
import Spawn (initSpawn)
import Data
import Foreign.Object as F

main ∷ Effect Unit
main = do
  memory     ← Memory.getMemoryGlobal
  loopStatus ← Memory.get memory "loopStatus"
  let ls = loopStatus ∷ Either JsonDecodeError (Maybe LoopStatus)
  case ls of
    Left err → log $ printJsonDecodeError err
    Right status → case status of
      Nothing  → Memory.set memory "loopStatus" LoopStart
      Just ls0 → runCorpsegrinder ls0 memory

runCorpsegrinder ∷ LoopStatus → MemoryGlobal → Effect Unit
runCorpsegrinder LoopStart       memory = do
  log "starting the corpsegrinder..."
  Memory.set memory "loopStatus" LoopGo
  Memory.set memory "utility"    0
  game ← Game.getGameGlobal
  initSpawn game
  manageCreeps game memory
runCorpsegrinder LoopGo          memory = do
  game ← Game.getGameGlobal
  -- TODO: figure out how to reset the game memory
  -- check if there is a spawn, if not reset
--   if (F.size (Game.spawns game)) < 1 
--     then Memory.set memory "loopStatus" LoopReset
--     else pure unit
  let time = Game.time game
      modT = time `mod` 12
  -- the following functions will get called once every 12 ticks
  case modT of
    0 → freeCreepMemory game memory
    3 → manageCreeps game memory
    6 → processCreeps game memory
    -- 9 → ???
    _ → pure unit
  preformCreeps game memory
runCorpsegrinder LoopReset       memory = do
  log $ "resetting the corpsegrinder..."
  Memory.clear memory
  Memory.set memory "loopStatus" LoopGo
  Memory.set memory "utility"    0
  game ← Game.getGameGlobal
  initSpawn game
  manageCreeps game memory
runCorpsegrinder (LoopError str) _      = log $ "Error: " <> str
runCorpsegrinder LoopNULL        _      = pure unit

freeCreepMemory ∷ GameGlobal → MemoryGlobal → Effect Unit
freeCreepMemory game memory = do
  creeps' ← Memory.get memory "creeps"
  let creeps = case creeps'' of
                 Left  _  → []
                 Right c0 → F.keys c0
      creeps'' = creeps' ∷ Either JsonDecodeError (F.Object Json)
  freeCreepMemoryF game memory creeps
freeCreepMemoryF ∷ GameGlobal → MemoryGlobal → Array String → Effect Unit
freeCreepMemoryF _    _      []     = pure unit
freeCreepMemoryF game memory creeps = do
  if creepN `F.member` (Game.creeps game) then freeCreepMemoryF game memory creeps'
  else do
    log $ "freeing creep " <> creepN <> "..."
    Memory.freeCreep memory creepN
    freeCreepMemoryF game memory creeps'
  where creeps' = case uncons creeps of
                    Just {head: _, tail: cs} → cs
                    Nothing                  → []
        creepN  = case uncons creeps of
                    Just {head: c, tail: _}  → c
                    Nothing                  → ""
