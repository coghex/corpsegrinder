module UPrelude ( module Prelude, (≡)) where
-- ^ i define some extra universal stuff, like more unicode
import Prelude
import Data.Eq(eq)

-- these should be in the default prelude
infix 4 eq as ≡
