module UPrelude
  ( module P
  , (≡), (≠), (≤), (≥), (∧), (∨))
  where
-- ^ i define some extra universal stuff, like more unicode
import Prelude as P
import Data.Eq(eq,notEq)
import Data.Ord(lessThanOrEq,greaterThanOrEq)
import Data.BooleanAlgebra(conj,disj)

-- these should be in the default prelude
infix 4 eq              as ≡
infix 4 notEq           as ≠
infix 4 lessThanOrEq    as ≤
infix 4 greaterThanOrEq as ≥
infix 3 conj            as ∧
infix 2 disj            as ∨
-- these operators have nonstandard libraries (like data.array and data.list)
-- so they would need to be implemented on a case by case basis
-- infix 5 concat          as ⧺
-- infix 9 index           as ‼
