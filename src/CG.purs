module CG where

import UPrelude
import Screeps.Data
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Control.Lazy (class Lazy)
import Data.DateTime as DT
import Data.Date as Date
import Data.Time as Time
import Screeps.Memory as Memory
import Screeps.Game   as Game
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Category (identity)
import Data.Newtype (class Newtype)

-- read only environment
type Env = { memory ∷ MemoryGlobal, game ∷ GameGlobal }

-- continuation monad
newtype CG ε α = CG (Env → Effect α)
runCG ∷ ∀ α. CG Env α → Env → Effect α
runCG (CG cg) = cg

-- Log data
data LogLevel = LogDebug | LogInfo | LogWarn | LogError | LogNULL
derive instance eqLogLevel   ∷ Eq   LogLevel
derive instance ordLogLevel  ∷ Ord  LogLevel
instance showLogLevel ∷ Show LogLevel where
  show LogDebug = "Debug"
  show LogInfo  = "Info"
  show LogWarn  = "Warn"
  show LogError = "Error"
  show LogNULL  = "NULL"
type LogStr = { level ∷ LogLevel
              , time  ∷ DT.DateTime
              , msg   ∷ String }

-- logger class
class MonadLog μ where
  logIO ∷ LogStr → μ Unit

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

instance monadLogCG ∷ MonadLog (CG ε) where
  logIO {level:lvl,time:t,msg:m}
    = liftEffect $ log $ (show lvl) <> ": [" <> (format t) <> "]: " <> m

format ∷ DT.DateTime → String
format dt@(DT.DateTime d t) = (show day)  <> "/" <> (show month) <> "/" <> (show year)
             <> ": " <> (show hour) <> ":" <> (show min)   <> ":" <> (show sec)
  where day   = Date.day    d
        month = Date.month  d
        year  = Date.year   d
        hour  = Time.hour   t
        min   = Time.minute t
        sec   = Time.second t

log' ∷ LogLevel → String → CG Env Unit
log' lvl str = liftEffect nowDateTime >>= logIO <<< { level: lvl, time: _, msg: str }
