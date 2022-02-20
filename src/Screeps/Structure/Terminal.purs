module Screeps.Structure.Terminal where

import UPrelude
import Effect (Effect)
import Screeps.Data
import Screeps.FFI

cooldown ∷ Terminal → Int
cooldown = unsafeField "cooldown"

store ∷ Terminal → Store
store = unsafeField "store"

send ∷ Terminal → ResourceType → Int → String → Effect ReturnCode
send = runThisEffFn3 "send"

send' ∷ Terminal → ResourceType → Int → String → String → Effect ReturnCode
send' = runThisEffFn4 "send"
