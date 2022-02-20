module Screeps.ConstructionSite where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

id ∷ ConstructionSite → Id ConstructionSite
id = unsafeField "id"

my ∷ ConstructionSite → Boolean
my = unsafeField "my"

owner ∷ ConstructionSite → { username ∷ String }
owner = unsafeField "owner"

progress ∷ ConstructionSite → Int
progress = unsafeField "progress"

progressTotal ∷ ConstructionSite → Int
progressTotal = unsafeField "progressTotal"

structureType ∷ ConstructionSite → StructureType
structureType = unsafeField "structureType"

remove ∷ ConstructionSite → Effect ReturnCode
remove = runThisEffFn0 "remove"
