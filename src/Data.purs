module Data where

import UPrelude
import Data.String ( length, take, drop )
import Data.Either ( note )
import Data.Argonaut.Core ( jsonEmptyObject )
import Data.Argonaut.Encode ( class EncodeJson, encodeJson
                            , (:=), (~>))
import Data.Argonaut.Decode ( class DecodeJson, decodeJson
                            , JsonDecodeError(..), (.:) )
import Screeps.Data

-- | defines the status of the main loop
data LoopStatus = LoopGo
                | LoopStart
                | LoopReset
                | LoopError String
                | LoopNULL
instance showLoopStatus ∷ Show LoopStatus where
  show LoopGo          = "LoopGo"
  show LoopStart       = "LoopStart"
  show LoopReset       = "LoopReset"
  show (LoopError err) = "LoopError:" <> err
  show LoopNULL        = "LoopNULL"
instance encodeLoopStatus ∷ EncodeJson LoopStatus where
  encodeJson ls = encodeJson $ show ls
instance decodeLoopStatus ∷ DecodeJson LoopStatus where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "LoopStatus") (lsFromStr string)
lsFromStr ∷ String → Maybe LoopStatus
lsFromStr "LoopGo"    = Just LoopGo
lsFromStr "LoopStart" = Just LoopStart
lsFromStr "LoopReset" = Just LoopReset
lsFromStr "LoopNULL"  = Just LoopNULL
lsFromStr str         = if (length str) > 9 then
    if (take 9 str) ≡ "LoopError" then
      Just $ LoopError (drop 9 str)
    else Nothing
  else Nothing

-- | possible log levels
data LogLevel = LogDebug | LogInfo | LogWarn | LogError | LogNULL
derive instance eqLogLevel  ∷ Eq  LogLevel
derive instance ordLogLevel ∷ Ord LogLevel
instance showLogLevel ∷ Show LogLevel where
  show LogDebug = "Debug"
  show LogInfo  = "Info"
  show LogWarn  = "Warn"
  show LogError = "Error"
  show LogNULL  = "NULL"

-- | creeps have a static type that dictates structure and possible roles
data CreepType = CreepPeon
               | CreepCollier
               | CreepHauler
               | CreepGrunt
               | CreepNULL
instance showCreepType ∷ Show CreepType where
  show CreepPeon    = "CreepPeon"
  show CreepCollier = "CreepCollier"
  show CreepHauler  = "CreepHauler"
  show CreepGrunt   = "CreepGrunt"
  show CreepNULL    = "CreepNULL"
instance eqCreepType ∷ Eq CreepType where
  eq CreepPeon    CreepPeon    = true
  eq CreepCollier CreepCollier = true
  eq CreepHauler  CreepHauler  = true
  eq CreepGrunt   CreepGrunt   = true
  eq CreepNULL    CreepNULL    = true
  eq _            _            = false
instance encodeCreepType ∷ EncodeJson CreepType where
  encodeJson creeptype = encodeJson $ show creeptype
instance decodeCreepType ∷ DecodeJson CreepType where
  decodeJson json = do
    string ← decodeJson json
    note (TypeMismatch "DecodeJson") (ctFromStr string)
ctFromStr ∷ String → Maybe CreepType
ctFromStr "CreepPeon"    = Just CreepPeon
ctFromStr "CreepCollier" = Just CreepCollier
ctFromStr "CreepHauler"  = Just CreepHauler
ctFromStr "CreepGrunt"   = Just CreepGrunt
ctFromStr "CreepNULL"    = Just CreepNULL
ctFromStr _              = Nothing

-- | roles are sets of behaviors that creeps can choose to play
data Role = RoleIdle
          | RoleBuilder
          | RoleHarvester
          | RoleUpgrader
          | RoleWorker Job
          | RoleCollier
          | RoleNULL
roleList = [RoleIdle, RoleHarvester, RoleBuilder, (RoleWorker JobNULL)
           , RoleCollier, RoleUpgrader, RoleNULL] ∷ Array Role
instance showRole ∷ Show Role where
  show RoleHarvester  = "RoleHarvester"
  show RoleUpgrader   = "RoleUpgrader"
  show RoleBuilder    = "RoleBuilder"
  show (RoleWorker j) = "RoleWorker:" <> (show j)
  show RoleCollier    = "RoleCollier"
  show RoleIdle       = "RoleIdle"
  show RoleNULL       = "RoleNULL"
instance eqRoles ∷ Eq Role where
  eq RoleHarvester  RoleHarvester  = true
  eq RoleUpgrader   RoleUpgrader   = true
  eq RoleBuilder    RoleBuilder    = true
  eq RoleCollier    RoleCollier    = true
  eq (RoleWorker j) (RoleWorker k) = j ≡ k
  eq RoleIdle       RoleIdle       = true
  eq RoleNULL       RoleNULL       = true
  eq _              _              = false
instance encodeRole ∷ EncodeJson Role where
  encodeJson role = encodeJson $ show role
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
    if (take 10 str) ≡ "RoleWorker" then
      case take 9 (drop 11 str) of
        "JobRepair" → Just $ RoleWorker $ JobRepair $ drop 21 str
        _           → Just $ RoleWorker JobNULL
    else Nothing
  else Nothing

-- | jobs provide a one time behavior for a worker role to play
data Job = JobNULL
         | JobRepair String
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

-- | Spots are the most abstract form of tile on the map
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
-- | spots are always one of four types
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

-- | HarvestSpot holds harvest data for a source
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

-- | values held in memory for each container
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

-- | return structure for certain functions
data CreepCounts = CreepCounts
  { nPeon    ∷ Int
  , nCollier ∷ Int
  , nHauler  ∷ Int
  , nGrunt   ∷ Int }

