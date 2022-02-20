module Main where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Argonaut.Decode (printJsonDecodeError, JsonDecodeError)
import Screeps.Memory as Memory
import Screeps.Data
import Data

main ∷ Effect Unit
main = do
  memory     ← Memory.getMemoryGlobal
  loopStatus ← Memory.get memory "loopStatus"
  let ls = loopStatus ∷ Either JsonDecodeError (Maybe LoopStatus)
  case loopStatus of
    Left err → do
      log $ printJsonDecodeError err
      pure unit
    Right status → case status of
      Nothing → initLoop memory
      Just ls → log $ "loopStatus: " <> (show status)

initLoop ∷ MemoryGlobal → Effect Unit
initLoop memory = do
  log "starting the corpsegrinder..."
  Memory.set memory "loopStatus" $ LoopGo
