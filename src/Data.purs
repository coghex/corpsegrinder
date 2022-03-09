module Data where

import UPrelude
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson
                            , JsonDecodeError(..), (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Int ( fromString )
import Data.String ( length, take, drop )
import Data.Either ( note )
import Screeps.Data

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

data CreepType = CreepPeon | CreepCollier | CreepHauler | CreepGrunt | CreepNULL
instance encodeCreepType ∷ EncodeJson CreepType where
  encodeJson CreepPeon    = encodeJson "creepPeon"
  encodeJson CreepCollier = encodeJson "creepCollier"
  encodeJson CreepHauler  = encodeJson "creepHauler"
  encodeJson CreepGrunt   = encodeJson "creepGrunt"
  encodeJson CreepNULL    = encodeJson "creepNULL"
instance decodeCreepType ∷ DecodeJson CreepType where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "DecodeJson") (ctFromStr string)
ctFromStr ∷ String → Maybe CreepType
ctFromStr "creepPeon"    = Just CreepPeon
ctFromStr "creepCollier" = Just CreepCollier
ctFromStr "creepHauler"  = Just CreepHauler
ctFromStr "creepGrunt"   = Just CreepGrunt
ctFromStr "creepNULL"    = Just CreepNULL
ctFromStr _              = Nothing
instance eqCreepType ∷ Eq CreepType where
  eq CreepPeon    CreepPeon    = true
  eq CreepCollier CreepCollier = true
  eq CreepHauler  CreepHauler  = true
  eq CreepGrunt   CreepGrunt   = true
  eq CreepNULL    CreepNULL    = true
  eq _            _            = false

data CreepCounts = CreepCounts
  { nPeon    ∷ Int
  , nCollier ∷ Int
  , nHauler  ∷ Int
  , nGrunt   ∷ Int }

data Job       = JobNULL | JobRepair String
instance showJob ∷ Show Job where
  show JobNULL       = "JobNULL"
  show (JobRepair n) = "JobRepair:" <> (show n)
instance eqJob ∷ Eq Job where
  eq JobNULL       JobNULL       = true
  eq (JobRepair _) (JobRepair _) = true
  eq _             _             = false
instance encodeJob ∷ EncodeJson Job where
  encodeJson JobNULL       = encodeJson "JobNULL"
  encodeJson (JobRepair n) = encodeJson $ "JobRepair:" <> (show n)
instance decodeJob ∷ DecodeJson Job where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "Job") (jobFromStr string)
jobFromStr ∷ String → Maybe Job
jobFromStr "JobNULL" = Just JobNULL
jobFromStr str       = if (length str) > 9 then
    if (take 9 str) ≡ "JobRepair" then Just $ JobRepair $ drop 10 str
    else Nothing
  else Nothing

data Role      = RoleIdle
               | RoleBuilder Int
               | RoleHarvester
               | RoleUpgrader
               | RoleWorker Job
               | RoleCollier
               | RoleNULL
instance showRole ∷ Show Role where
  show RoleHarvester   = "RoleHarvester"
  show RoleUpgrader    = "RoleUpgrader"
  show (RoleBuilder n) = "RoleBuilder " <> (show n)
  show (RoleWorker _)  = "RoleWorker"
  show RoleCollier     = "RoleCollier"
  show RoleIdle        = "RoleIdle"
  show RoleNULL        = "RoleNULL"
instance eqRoles ∷ Eq Role where
  eq RoleNULL        RoleNULL        = true
  eq RoleIdle        RoleIdle        = true
  eq RoleHarvester   RoleHarvester   = true
  eq RoleUpgrader    RoleUpgrader    = true
  eq (RoleWorker _)  (RoleWorker _)  = true
  eq (RoleBuilder _) (RoleBuilder _) = true
  eq RoleCollier     RoleCollier     = true
  eq _               _               = false
instance encodeRole ∷ EncodeJson Role where
  encodeJson RoleIdle        = encodeJson "RoleIdle"
  encodeJson RoleHarvester   = encodeJson "RoleHarvester"
  encodeJson RoleUpgrader    = encodeJson "RoleUpgrader"
  encodeJson (RoleBuilder n) = encodeJson $ "RoleBuilder:" <> (show n)
  encodeJson (RoleWorker n)  = encodeJson $ "RoleWorker:" <> (show n)
  encodeJson RoleCollier     = encodeJson "RoleCollier"
  encodeJson RoleNULL        = encodeJson "RoleNULL"
instance decodeRole ∷ DecodeJson Role where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "Role") (roleFromStr string)
roleFromStr ∷ String → Maybe Role
roleFromStr "RoleHarvester" = Just RoleHarvester
roleFromStr "RoleUpgrader"  = Just RoleUpgrader
roleFromStr "RoleIdle"      = Just RoleIdle
roleFromStr "RoleCollier"   = Just RoleCollier
roleFromStr "RoleNULL"      = Just RoleNULL
roleFromStr str             = if (length str) > 11 then
    if (take 11 str) ≡ "RoleBuilder" then
      case (fromString (drop 12 str)) of
        Nothing → Nothing
        Just s0 → Just $ RoleBuilder s0
    else if (take 10 str) ≡ "RoleWorker" then
      case (take 9 (drop 11 str)) of
        "JobRepair" → Just $ RoleWorker $ JobRepair $ drop 21 str
        _           → Just $ RoleWorker JobNULL
    else Nothing
  else Nothing

roleList = [RoleIdle, RoleHarvester, (RoleBuilder 0), (RoleWorker JobNULL)
           , RoleCollier, RoleUpgrader, RoleNULL] ∷ Array Role

data HarvestSpot = HarvestSpot { sourceName ∷ Id Source
                               , nHarvs     ∷ Int
                               , nMaxHarvs  ∷ Int
                               , harvSpots  ∷ Array Spot }
instance encodeHarvestSpot ∷ EncodeJson HarvestSpot where
  encodeJson (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }) =
    "sourceName"     := sourceName
      ~> "nHarvs"    := nHarvs
      ~> "nMaxHarvs" := nMaxHarvs
      ~> "harvSpots" := harvSpots
      ~> jsonEmptyObject
instance decodeHarvestSpot ∷ DecodeJson HarvestSpot where
  decodeJson json = do
    obj ← decodeJson json
    sourceName ← obj .: "sourceName"
    nHarvs     ← obj .: "nHarvs"
    nMaxHarvs  ← obj .: "nMaxHarvs"
    harvSpots  ← obj .: "harvSpots"
    pure $ HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }
data Spot = Spot { spotType ∷ SpotType
                 , spotX    ∷ Int
                 , spotY    ∷ Int }
instance encodeSpot ∷ EncodeJson Spot where
  encodeJson (Spot {spotType, spotX, spotY})  =
    "spotType"      := spotType
      ~> "spotX"    := spotX
      ~> "spotY"    := spotY
      ~> jsonEmptyObject
instance decodeJson ∷ DecodeJson Spot where
  decodeJson json = do
    obj ← decodeJson json
    spotType ← obj .: "spotType"
    spotX    ← obj .: "spotX"
    spotY    ← obj .: "spotY"
    pure $ Spot { spotType, spotX, spotY }
instance spotEq ∷ Eq Spot where
  eq (Spot {spotType:_,spotX:x0,spotY:y0})
     (Spot {spotType:_,spotX:x1,spotY:y1}) = (x0 ≡ x1) ∧ (y0 ≡ y1)

data SpotType = SpotPlain | SpotWall | SpotSwamp | SpotLava
instance encodeSpotType ∷ EncodeJson SpotType where
  encodeJson SpotPlain = encodeJson "spotPlain"
  encodeJson SpotWall  = encodeJson "spotWall"
  encodeJson SpotSwamp = encodeJson "spotSwamp"
  encodeJson SpotLava  = encodeJson "spotLava"
instance decodeSpotType ∷ DecodeJson SpotType where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "Spot") (spotTypeFromStr string)
spotTypeFromStr ∷ String → Maybe SpotType
spotTypeFromStr "spotPlain" = Just SpotPlain
spotTypeFromStr "spotWall"  = Just SpotWall
spotTypeFromStr "spotSwamp" = Just SpotSwamp
spotTypeFromStr "spotLava"  = Just SpotLava
spotTypeFromStr _           = Nothing

data ContainerMemory = ContainerMemory
  { id   ∷ Id Container 
  , used ∷ Boolean }
-- containers are the same if they have the same id
instance eqContainerMemory ∷ Eq ContainerMemory where
  eq (ContainerMemory {id:id0,used:u0})
     (ContainerMemory {id:id1,used:u1}) = id0 ≡ id1
instance encodeContainerMemory ∷ EncodeJson ContainerMemory where
  encodeJson (ContainerMemory {id,used}) =
    "id"        := id
      ~> "used" := used
      ~> jsonEmptyObject
instance decodeContainerMemory ∷ DecodeJson ContainerMemory where
  decodeJson json = do
    obj  ← decodeJson json
    id   ← obj .: "id"
    used ← obj .: "used"
    pure $ ContainerMemory { id, used }
