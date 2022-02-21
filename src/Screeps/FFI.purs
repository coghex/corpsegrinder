module Screeps.FFI where
import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..), isJust, fromJust, maybe)
import Data.Function.Uncurried ( Fn3, runFn3 )
import Partial.Unsafe (unsafePartial)

foreign import unsafeField ∷ ∀ obj val. String → obj → val
foreign import unsafeGetFieldEff ∷ ∀ obj val. String → obj → Effect val
foreign import unsafeSetFieldEff ∷ ∀ obj val. String → obj → val → Effect Unit
foreign import unsafeSetFieldKeyEff ∷ ∀ obj val. String → obj → String → val → Effect Unit
foreign import unsafeDeleteFieldEff ∷ ∀ obj. String → obj → Effect Unit
foreign import unsafeDeleteFieldKeyEff ∷ ∀ obj. String → String → obj → Effect Unit
foreign import runThisEffFn0 ∷ ∀ this α. String → this → Effect α
foreign import runThisEffFn1 ∷ ∀ this α β. String → this → α → Effect β
foreign import runThisEffFn2 ∷ ∀ this α β γ. String → this → α → β → Effect γ
foreign import runThisEffFn3 ∷ ∀ this α β γ δ. String
      → this → α → β → γ → Effect δ
foreign import runThisEffFn4 ∷ ∀ this α β γ δ ε. String
      → this → α → β → γ → δ → Effect ε
foreign import runThisFn0 ∷ ∀ this α. String → this → α
foreign import runThisFn1 ∷ ∀ this α β. String → this → α → β
foreign import runThisFn2 ∷ ∀ this α β γ. String → this → α → β → γ
foreign import runThisFn3 ∷ ∀ this α β γ δ. String → this → α → β → γ → δ

foreign import data NullOrUndefined ∷ Type → Type
foreign import null ∷ ∀ α. NullOrUndefined α
foreign import undefined ∷ ∀ α. NullOrUndefined α
foreign import notNullOrUndefined ∷ ∀ α. α → NullOrUndefined α
foreign import isNull ∷ ∀ α. NullOrUndefined α → Boolean
foreign import isUndefined ∷ ∀ α. NullOrUndefined α → Boolean
foreign import toMaybeImpl ∷ ∀ α m. Fn3 (NullOrUndefined α) m (α → m) m

toNullable ∷ ∀ α. Maybe α → NullOrUndefined α
toNullable = maybe null notNullOrUndefined

toMaybe ∷ ∀ α. NullOrUndefined α → Maybe α
toMaybe n = runFn3 toMaybeImpl n Nothing Just

foreign import data JsObject ∷ Type
foreign import selectMaybesImpl ∷ ∀ α. (Maybe α → Boolean)
  → (Maybe α → α) → α → JsObject

selectMaybes ∷ ∀ α. α → JsObject
selectMaybes obj = unsafePartial $ selectMaybesImpl isJust fromJust obj
