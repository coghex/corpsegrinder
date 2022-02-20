module Screeps.Memory where
import UPrelude
import Effect (Effect)
import Screeps.FFI ( unsafeGetFieldEff, unsafeSetFieldEff
                   , unsafeDeleteFieldEff, unsafeDeleteFieldKeyEff )
import Screeps.Creep ( unsafeDeleteCreepEff )
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError)
import Data.Either (Either)
import Screeps.Data (MemoryGlobal, RawMemoryGlobal)

foreign import getMemoryGlobal ∷ Effect MemoryGlobal
foreign import getRawMemoryGlobal ∷ Effect RawMemoryGlobal

set ∷ ∀ α. (EncodeJson α) ⇒ MemoryGlobal → String
  → α → Effect Unit
set memoryGlobal key val
  = unsafeSetFieldEff key memoryGlobal $ encodeJson val
get ∷ ∀ α. (DecodeJson α) ⇒ MemoryGlobal → String
  → Effect (Either JsonDecodeError α)
get memoryGlobal key
  = decodeJson <$> unsafeGetFieldEff key memoryGlobal
free ∷ MemoryGlobal → String → Effect Unit
free memoryGlobal key
  = unsafeDeleteFieldEff key memoryGlobal
-- TODO: find out why this doesnt work
freeField ∷ MemoryGlobal → String → String → Effect Unit
freeField memoryGlobal obj key
  = unsafeDeleteFieldKeyEff obj key memoryGlobal
freeCreep ∷ MemoryGlobal → String → Effect Unit
freeCreep memoryGlobal creep
 = unsafeDeleteCreepEff creep memoryGlobal
