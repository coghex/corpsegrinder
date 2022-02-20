module Screeps.Structure.Spawn where

import UPrelude
import Effect (Effect)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Screeps.Data
import Screeps.FFI
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

foreign import createCreepImpl ∷ Spawn → Array BodyPartType
  → (ReturnCode → Either ReturnCode String)
  → (String → Either ReturnCode String)
  → Effect (Either ReturnCode String)
foreign import createCreepPrimeImpl ∷ ∀ mem. Spawn → Array BodyPartType
  → NullOrUndefined String → mem → (ReturnCode → Either ReturnCode String)
  → (String → Either ReturnCode String) → Effect (Either ReturnCode String)

memory ∷ ∀ α. Spawn → { | α }
memory = unsafeField "memory"

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
