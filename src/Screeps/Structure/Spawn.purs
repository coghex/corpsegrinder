module Screeps.Structure.Spawn where

import UPrelude
import Effect (Effect)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError)
import Screeps.Data
import Screeps.FFI
import Foreign.Object as F

foreign import createCreepImpl ∷ Spawn → Array BodyPartType
  → (ReturnCode → Either ReturnCode String)
  → (String → Either ReturnCode String)
  → Effect (Either ReturnCode String)
foreign import createCreepPrimeImpl ∷ ∀ mem. Spawn → Array BodyPartType
  → NullOrUndefined String → mem → (ReturnCode → Either ReturnCode String)
  → (String → Either ReturnCode String) → Effect (Either ReturnCode String)

memory ∷ Spawn → F.Object Json --∀ α. Spawn → { | α }
memory = unsafeField "memory"

setMemory ∷ ∀ α. (EncodeJson α) ⇒ Spawn → String → α → Effect Unit
setMemory spawn key val = unsafeSetFieldEff key spawnMemory $ encodeJson val
  where spawnMemory = unsafeField "memory" spawn

getMemory ∷ ∀ α. (DecodeJson α) ⇒ Spawn → String → Effect (Either JsonDecodeError α)
getMemory spawn key = decodeJson <$> unsafeGetFieldEff key spawnMemory
  where spawnMemory = unsafeField "memory" spawn

name ∷ Spawn → String
name = unsafeField "name"

spawning ∷ Spawn → Spawning
spawning = unsafeField "spawning"

store ∷ Spawn → Store
store = unsafeField "store"

--TODO: add options
spawnCreep ∷ Spawn → Array BodyPartType → Effect (Either ReturnCode String)
spawnCreep spawn parts = createCreepImpl spawn parts Left Right

spawnCreep' ∷ ∀ mem. (EncodeJson mem) ⇒ Spawn → Array BodyPartType
  → Maybe String → mem → Effect (Either ReturnCode String)
spawnCreep' spawn parts name' mem =
  createCreepPrimeImpl spawn parts (toNullable name') (encodeJson mem) Left Right

recycleCreep ∷ Spawn → Creep → Effect ReturnCode
recycleCreep = runThisEffFn1 "recycleCreep"

renewCreep ∷ Spawn → Creep → Effect ReturnCode
renewCreep = runThisEffFn1 "renewCreep"
