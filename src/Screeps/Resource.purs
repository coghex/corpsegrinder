module Screeps.Resource where

import UPrelude
import Screeps.Data
import Screeps.FFI

amount ∷ Resource → Int
amount = unsafeField "amount"

id ∷ Resource → Id Resource
id = unsafeField "id"

resourceType ∷ Resource → ResourceType
resourceType = unsafeField "resourceType"
