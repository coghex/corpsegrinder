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
  let creeps = Game.creeps game
  --initSpawn1 creeps game memory
  manageCreeps creeps game memory
runCorpsegrinder LoopGo          memory = do
  game ← Game.getGameGlobal
  let time = Game.time game
  if (time `mod` 12) ≡ 0 then do
    freeCreepMemory game memory
  else pure unit
runCorpsegrinder LoopReset       _      = do
  log $ "resetting the corpsegrinder..."
  -- TODO: reset the memory here
runCorpsegrinder (LoopError str) _      = log $ "Error: " <> str
runCorpsegrinder LoopNULL        _      = pure unit

freeCreepMemory ∷ GameGlobal → MemoryGlobal → Effect Unit
freeCreepMemory game memory = do
  creeps' ← Memory.get memory "creeps"
  let creeps = case creeps'' of
                 Left err → []
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
