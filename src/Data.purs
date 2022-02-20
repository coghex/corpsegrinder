module Data where

import UPrelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.String ( length, take, drop )
import Data.Either ( note )
import Data.Maybe ( Maybe(..) )

data    LoopStatus = LoopGo
                   | LoopStart
                   | LoopReset
                   | LoopError String
                   | LoopNULL

instance encodeLoopStatus ∷ EncodeJson LoopStatus where
  encodeJson LoopGo          = encodeJson   "loopGo"
  encodeJson LoopStart       = encodeJson   "loopStart"
  encodeJson LoopReset       = encodeJson   "loopReset"
  encodeJson (LoopError err) = encodeJson $ "loopError:" <> err
  encodeJson LoopNULL        = encodeJson   "loopNULL"
instance decodeLoopStatus ∷ DecodeJson LoopStatus where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "LoopStatus:") (lsFromStr string)
lsFromStr ∷ String → Maybe LoopStatus
lsFromStr "loopGo"    = Just LoopGo
lsFromStr "loopStart" = Just LoopStart
lsFromStr "loopReset" = Just LoopReset
lsFromStr "loopNULL"  = Just LoopNULL
lsFromStr str         = if (length str) > 9 then
    if (take 9 str) ≡ "loopError" then
      Just $ LoopError (drop 9 str)
    else Nothing
  else Nothing
instance showLoopStatus ∷ Show LoopStatus where
  show LoopGo          = "loopGo"
  show LoopStart       = "loopStart"
  show LoopReset       = "loopReset"
  show (LoopError err) = "loopError:" <> err
  show LoopNULL        = "loopNULL"

data CreepType = CreepDrone | CreepNULL
data Role      = RoleIdle
               | RoleBuilder
               | RoleHarvester
               | RoleNULL
data Job       = JobNULL
instance showRole ∷ Show Role where
  show RoleHarvester = "RoleHarvester"
  show RoleBuilder   = "RoleBuilder"
  show RoleIdle      = "RoleIdle"
  show RoleNULL      = "RoleNULL"
instance eqRoles ∷ Eq Role where
  eq RoleNULL      RoleNULL      = true
  eq RoleIdle      RoleIdle      = true
  eq RoleHarvester RoleHarvester = true
  eq RoleBuilder   RoleBuilder   = true
  eq _             RoleNULL      = false
  eq RoleNULL      _             = false
  eq _             _             = false
instance encodeRole ∷ EncodeJson Role where
  encodeJson RoleIdle      = encodeJson "RoleIdle"
  encodeJson RoleHarvester = encodeJson "RoleHarvester"
  encodeJson RoleBuilder   = encodeJson "RoleBuilder"
  encodeJson RoleNULL      = encodeJson "RoleNULL"
instance decodeRole ∷ DecodeJson Role where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "Role:") (roleFromStr string)
roleFromStr ∷ String → Maybe Role
roleFromStr "RoleHarvester" = Just RoleHarvester
roleFromStr "RoleBuilder"   = Just RoleBuilder
roleFromStr "RoleIdle"      = Just RoleIdle
roleFromStr "RoleNULL"      = Just RoleNULL
roleFromStr _               = Nothing
roleList = [RoleIdle, RoleHarvester, RoleBuilder, RoleNULL] ∷ Array Role

