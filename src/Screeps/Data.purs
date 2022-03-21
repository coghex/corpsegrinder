module Screeps.Data where

import UPrelude
import Data.Generic.Rep ( class Generic )
import Data.Eq ( class Eq )
import Data.Eq.Generic ( genericEq )
import Data.Show ( class Show )
import Data.Show.Generic ( genericShow )
import Data.Either ( note )
import Data.String ( length, take, drop )
import Data.Map as M
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..), getField)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)

-- return code/utility
newtype ReturnCode = ReturnCode Int
derive instance genericReturnCode ∷ Generic ReturnCode _
instance eqReturnCode             ∷ Eq      ReturnCode where eq = genericEq
instance showReturnCode           ∷ Show    ReturnCode where
  show (ReturnCode n) = show n
type    FilterFn α = α → Boolean
newtype FindType α = FindType Int
foreign import data Date ∷ Type
newtype Event = Event Int

-- memory
foreign import data MemoryGlobal ∷ Type
foreign import data RawMemoryGlobal ∷ Type

-- game
foreign import data GameGlobal ∷ Type
foreign import data WorldMap ∷ Type
type Cpu = { limit        ∷ Int
           , tickLimit    ∷ Int
           , bucket       ∷ Int
           , shardLimits  ∷ M.Map String Int
           , unlocked     ∷ Boolean
           , unlockedTime ∷ Int }
type Gcl = { level         ∷ Int
           , progress      ∷ Int
           , progressTotal ∷ Int }
type Gpl = { level         ∷ Int
           , progress      ∷ Int
           , progressTotal ∷ Int }
foreign import data InterShardMemory ∷ Type
type Shard = { shardName ∷ String
             , shardType ∷ String
             , shardPtr  ∷ Boolean }
type HeapStatistics =
  { total_heap_size            ∷ Int
  , total_heap_size_executable ∷ Int
  , total_physical_size        ∷ Int
  , total_available_size       ∷ Int
  , used_heap_size             ∷ Int
  , heap_size_limit            ∷ Int
  , malloced_memory            ∷ Int
  , peak_malloced_memory       ∷ Int
  , does_zap_garbage           ∷ Int
  , externally_allocated_size  ∷ Int }

-- room position
foreign import data RoomPosition ∷ Type
data TargetPosition α =
  TargetPt Int Int |
  TargetObj (RoomObject α) |
  TargetPos RoomPosition
type ExitsInfo =
  { "1" ∷ String
  , "3" ∷ String
  , "5" ∷ String
  , "7" ∷ String }

-- room
foreign import data Room ∷ Type
foreign import data RoomGlobal ∷ Type
data RoomIdentifier = RoomName String | RoomObj Room
type RoomStatus = { status ∷ String
                  , timestamp ∷ Int }

-- room terrain
foreign import data RoomTerrain ∷ Type
newtype TerrainMask = TerrainMask Int
derive instance genericTerrainMask ∷ Generic TerrainMask _
instance eqTerrainMask             ∷ Eq      TerrainMask where eq   = genericEq
instance showTerrainMask           ∷ Show    TerrainMask where show = genericShow
newtype Terrain = Terrain String
derive instance genericTerrain ∷ Generic Terrain _
instance eqTerrain             ∷ Eq      Terrain where eq = genericEq
instance showTerrain           ∷ Show    Terrain
  where show (Terrain s) = s

-- room object
foreign import data RawRoomObject ∷ Type → Type
type RoomObject α = RawRoomObject α
newtype Id a = Id String
derive instance genericId ∷ Generic (Id α) _
instance eqId ∷ Eq (Id α) where eq = genericEq
instance showId ∷ Show (Id α) where show = genericShow
instance decodeJsonId ∷ DecodeJson (Id α) where decodeJson = genericDecodeJson
instance encodeJsonId ∷ EncodeJson (Id α) where encodeJson = genericEncodeJson

-- construction site
foreign import data RawConstructionSite ∷ Type
type ConstructionSite = RoomObject RawConstructionSite

-- creep
foreign import data RawCreep ∷ Type
type Creep = RoomObject RawCreep
type BodyPart =
  { boost ∷ Maybe String
  , type  ∷ BodyPartType
  , hits  ∷ Int }
newtype BodyPartType = BodyPartType String
derive instance genericBodyPartType ∷ Generic BodyPartType _
instance eqBodyPartType             ∷ Eq      BodyPartType where eq   = genericEq
instance showBodyPartType           ∷ Show    BodyPartType where show = genericShow
type CreepInfo =
  { name ∷ String
  , needTime ∷ Int
  , remainingTime ∷ Int }

-- power creep
foreign import data RawPowerCreep ∷ Type
type PowerCreep = RoomObject RawPowerCreep
newtype PowerClass = PowerClass String

-- mineral
foreign import data RawMineral ∷ Type
type Mineral = RoomObject RawMineral
newtype Density = Density Int
-- resource
foreign import data RawResource ∷ Type
type Resource = RoomObject RawResource
newtype ResourceType = ResourceType String
derive instance genericResourceType ∷ Generic ResourceType _
instance eqResourceType             ∷ Eq      ResourceType where eq = genericEq
instance showResourceType           ∷ Show    ResourceType where
  show (ResourceType s) = s

-- deposit
foreign import data RawDeposit ∷ Type
type Deposit = RoomObject RawDeposit

-- source
foreign import data RawSource ∷ Type
type Source = RoomObject RawSource

-- nuke
foreign import data RawNuke ∷ Type
type Nuke = RoomObject RawNuke

-- flag
foreign import data RawFlag ∷ Type
type Flag = RoomObject RawFlag
newtype Color = Color Int
derive instance genericColor :: Generic Color _
instance eqColor   ∷ Eq   Color where eq   = genericEq
instance showColor ∷ Show Color where show = genericShow

-- structure
foreign import data RawStructure ∷ Type → Type
type Structure α = RoomObject (RawStructure α)
newtype StructureType = StructureType String
derive instance genericStructureType ∷ Generic StructureType _
instance eqStructureType             ∷ Eq      StructureType where eq   = genericEq
instance showStructureType           ∷ Show    StructureType where show = genericShow

-- owned structure
foreign import data RawOwnedStructure ∷ Type → Type
type OwnedStructure α = Structure (RawOwnedStructure α)

-- storage
foreign import data RawStorage ∷ Type
type Storage = OwnedStructure RawStorage

-- container
foreign import data RawContainer ∷ Type
type Container = OwnedStructure RawContainer

-- controller
foreign import data RawController ∷ Type
type Controller = OwnedStructure RawController
type Reservation =
  { username   ∷ String
  , ticksToEnd ∷ Int }
type Sign =
  { username   ∷ String
  , text       ∷ String
  , time       ∷ Int
  , dateTime   ∷ Date }

-- extension
foreign import data RawExtension ∷ Type
type Extension = OwnedStructure RawExtension

-- extractor
foreign import data RawExtractor ∷ Type
type Extractor = OwnedStructure RawExtractor

-- factory
foreign import data RawFactory ∷ Type
type Factory = OwnedStructure RawFactory

-- invader core
foreign import data RawInvaderCore ∷ Type
type InvaderCore = Structure RawInvaderCore

-- keeper lair
foreign import data RawKeeperLair ∷ Type
type KeeperLair = Structure RawKeeperLair

-- lab
foreign import data RawLab ∷ Type
type Lab = OwnedStructure RawLab

-- link
foreign import data RawLink ∷ Type
type Link = OwnedStructure RawLink

-- nuker
foreign import data RawNuker ∷ Type
type Nuker = OwnedStructure RawNuker

-- observer
foreign import data RawObserver ∷ Type
type Observer = OwnedStructure RawObserver

-- power bank
foreign import data RawPowerBank ∷ Type
type PowerBank = Structure RawPowerBank

-- power spawn
foreign import data RawPowerSpawn ∷ Type
type PowerSpawn = OwnedStructure RawPowerSpawn

-- portal
foreign import data RawPortal ∷ Type
type Portal = Structure RawPortal

-- rampart
foreign import data RawRampart ∷ Type
type Rampart = OwnedStructure RawRampart

-- road
foreign import data RawRoad ∷ Type
type Road = OwnedStructure RawRoad

-- spawn
foreign import data RawSpawn ∷ Type
type Spawn = OwnedStructure RawSpawn
foreign import data Spawning ∷ Type

-- tower
foreign import data RawTower ∷ Type
type Tower = OwnedStructure RawTower

-- terminal
foreign import data RawTerminal ∷ Type
type Terminal = OwnedStructure RawTerminal
foreign import data Market ∷ Type

-- wall
foreign import data RawWall ∷ Type
type Wall = OwnedStructure RawWall

-- flag
foreign import data RawRuin ∷ Type
type Ruin = RoomObject RawRuin

-- tombstone
foreign import data RawTombstone ∷ Type
type Tombstone = RoomObject RawTombstone

-- store
foreign import data Store ∷ Type

-- direction
newtype Direction = Direction Int
derive instance genericDirection    ∷ Generic    Direction _
instance        eqDirection         ∷ Eq         Direction where
  eq         = genericEq
instance        showDirection       ∷ Show       Direction where
  show       = genericShow
instance        decodeJsonDirection ∷ DecodeJson Direction where
  decodeJson = genericDecodeJson
instance        encodeJsonDirection ∷ EncodeJson Direction where
  encodeJson = genericEncodeJson
type Directions = Array Direction

-- path
type Path = Array PathStep
type PathStep =
  { x         ∷ Int
  , y         ∷ Int
  , dx        ∷ Number
  , dy        ∷ Number
  , direction ∷ Direction }
type MoveOptions = PathOptions
  ( reusePath       ∷ Maybe Int
  , serializeMemory ∷ Maybe Boolean
  , noPathFinding   ∷ Maybe Boolean )
moveOpts ∷ MoveOptions
moveOpts =
  { ignoreCreeps:                 Nothing
  , ignoreDestructibleStructures: Nothing
  , ignoreRoads:                  Nothing
  , ignore:                       Nothing
  , avoid:                        Nothing
  , maxOps:                       Nothing
  , heuristicWeight:              Nothing
  , serialize:                    Nothing
  , maxRooms:                     Nothing
  , reusePath:                    Nothing
  , serializeMemory:              Nothing
  , noPathFinding:                Nothing }
type PathOptions α =
  { ignoreCreeps                 ∷ Maybe Boolean
  , ignoreDestructibleStructures ∷ Maybe Boolean
  , ignoreRoads                  ∷ Maybe Boolean
  , ignore                       ∷ Maybe (Array RoomPosition)
  , avoid                        ∷ Maybe (Array RoomPosition)
  , maxOps                       ∷ Maybe Int
  , heuristicWeight              ∷ Maybe Number
  , serialize                    ∷ Maybe Boolean
  , maxRooms                     ∷ Maybe Int
  | α }
pathOpts ∷ PathOptions ()
pathOpts =
  { ignoreCreeps:                 Nothing
  , ignoreDestructibleStructures: Nothing
  , ignoreRoads:                  Nothing
  , ignore:                       Nothing
  , avoid:                        Nothing
  , maxOps:                       Nothing
  , heuristicWeight:              Nothing
  , serialize:                    Nothing
  , maxRooms:                     Nothing }
foreign import data Goal ∷ Type
foreign import data CostMatrix ∷ Type
foreign import data PathFinder ∷ Type
type RoomRoute = Array RoomExit
type RoomExit = { exit ∷ FindType Int, room ∷ String }
data ReturnPath = ReturnPath { path       ∷ Path
                             , ops        ∷ Int
                             , cost       ∷ Int
                             , incomplete ∷ Boolean }
