module Screeps.Structure where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

hits ∷ ∀ α. Structure α → Int
hits = unsafeField "hits"

hitsMax ∷ ∀ α. Structure α → Int
hitsMax = unsafeField "hitsMax"

id ∷ ∀ α. Structure α → Id (Structure α)
id = unsafeField "id"

structureType ∷ ∀ α. Structure α → StructureType
structureType = unsafeField "structureType"

destroy ∷ ∀ α. Structure α → Effect ReturnCode
destroy = runThisEffFn0 "destroy"

isActive ∷ ∀ α. Structure α → Effect ReturnCode
isActive = runThisEffFn0 "isActive"

notifyWhenAttacked ∷ ∀ α. Structure α → Boolean → Effect ReturnCode
notifyWhenAttacked = runThisEffFn1 "notifyWhenAttacked"
