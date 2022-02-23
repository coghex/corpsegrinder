module CG where

import UPrelude
import Screeps.Data
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Lazy (class Lazy)
import Screeps.Memory as Memory
import Screeps.Game   as Game
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Category (identity)
import Data.Newtype (class Newtype)

type Env = { memory ∷ MemoryGlobal, game ∷ GameGlobal }

newtype CG ε α = CG (Env → Effect α)
runCG ∷ ∀ α. CG Env α → Env → Effect α
runCG (CG cg) = cg

-- i dont know why i do this part
derive instance newtypeCG ∷ Newtype (CG ε α) _

-- derive some instances to make it a monad
derive instance functorCG ∷ Functor (CG ε)
instance applicativeCG ∷ Applicative (CG ε) where
  pure a = CG $ \_ → pure $ a
instance monadCG ∷ Monad (CG ε)
instance bindCG ∷ Bind (CG ε) where
  bind (CG m) f = CG \r → m r >>= \a → case f a of
    CG f' → f' r <#> \res → res
instance applyCG ∷ Apply (CG ε) where
  apply (CG f) (CG m) = CG \r → f r >>= \f' → m r <#> \a'' → f' a''
-- instance monadTransCG ∷ MonadTrans (CG Env) where
--   lift m = CG \_ → m >>= \a → pure $ CGResult a
-- instance lazyCG ∷ Lazy (CG Env m α) where
--   defer f = CG \r → case f unit of CG f' → f' r
instance monadEffectCG ∷ MonadEffect (CG ε) where
  liftEffect m = CG $ \_ → m
-- reader
instance monadAskCG    ∷ MonadAsk    Env   (CG ε) where
  ask       = CG $ \e → pure e
  -- TODO: fill in this placeholder
instance monadReaderCG ∷ MonadReader Env   (CG ε) where
  local f m = m
