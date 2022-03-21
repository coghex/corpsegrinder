module Util where

import UPrelude
import Data.Array (index, head, tail
                  , uncons, zip, length
                  , deleteAt)
import Data.Int ( quot )
import Data.Tuple ( Tuple(..) )
import Data.Argonaut.Core ( Json )
import Data.Argonaut.Decode ( getField )
import Control.Monad.Reader ( asks )
import Screeps.Data
import Screeps.Room             as Room
import Screeps.RoomObject       as RO
import Screeps.RoomPosition     as RP
import Screeps.Store            as Store
import Screeps.Structure ( structureType )
import Screeps.ConstructionSite as CS
import Screeps.Const ( resource_energy, find_my_construction_sites
                     , find_structures, structure_container
                     , terrain_mask_wall )
import Foreign.Object as F
import Maths ( removeVal, distance, findMin )
import Data
import Spawn
import CorpseGrinder


-- | sets harvest spot memory when creep targets source
setNHarvs ∷ Array HarvestSpot → Id Source → Spawn → CorpseGrinder Env Unit
setNHarvs harvs sourceId spawn = do
  let newHarvs = map (setNHarvsF sourceId) harvs
  setSpawnMem spawn "harvestSpots" newHarvs
setNHarvsF ∷ Id Source → HarvestSpot → HarvestSpot
setNHarvsF sourceId (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }) =
  HarvestSpot { sourceName: sourceName, nHarvs:    (nHarvs + n)
              , nMaxHarvs:  nMaxHarvs,  harvSpots: harvSpots }
  where n = if (sourceId ≡ sourceName) then 1 else 0
-- | remove a harvester from a spawn's memory and sets any containers ton not work
removeNHarvs' ∷ Array HarvestSpot → Id Source → Spwn Unit
removeNHarvs' harvs0 sourceId = do
  spawn ← asks (_.spawn)
  let containers = Room.find' (RO.room spawn) find_structures
                              $ structIsType structure_container
      newHarvs1 = map (removeNHarvsF sourceId) harvs0
      newHarvs2 = map (removeContainerHarvSpots containers) newHarvs1
  setSpawnMem' "harvestSpots" newHarvs2
removeNHarvsF ∷ Id Source → HarvestSpot → HarvestSpot
removeNHarvsF sourceId (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }) =
  HarvestSpot { sourceName: sourceName, nHarvs:    (nHarvs - n)
              , nMaxHarvs:  nMaxHarvs,  harvSpots: harvSpots }
  where n = if (sourceId ≡ sourceName) then 1 else 0
-- | lifted version
removeNHarvs ∷ Array HarvestSpot → Id Source → Spawn → CorpseGrinder Env Unit
removeNHarvs harvs0 sourceId spawn = do
  let containers = Room.find' (RO.room spawn) find_structures
                              $ structIsType structure_container
      newHarvs1 = map (removeNHarvsF sourceId) harvs0
      newHarvs2 = map (removeContainerHarvSpots containers) newHarvs1
  setSpawnMem spawn "harvestSpots" newHarvs2

removeContainerHarvSpots ∷ Array (Structure Container) → HarvestSpot → HarvestSpot
removeContainerHarvSpots []         harvestSpots = harvestSpots
removeContainerHarvSpots containers harvestSpots =
  removeContainerHarvSpots containers' harvestSpots'
  where containers' = case uncons containers of
                        Nothing              → []
                        Just {head:_,tail:t} → t
        harvestSpots' = case uncons containers of
                          Nothing              → harvestSpots
                          Just {head:h,tail:_} → checkHarvestSpot h harvestSpots 


checkHarvestSpot ∷ Structure Container → HarvestSpot → HarvestSpot
checkHarvestSpot container (HarvestSpot { sourceName,nHarvs,nMaxHarvs,harvSpots }) =
  case containerInHarvestSpot containerPos harvSpots of
    Nothing → HarvestSpot { sourceName: sourceName, nHarvs: nHarvs
                          , nMaxHarvs: nMaxHarvs, harvSpots: harvSpots }
    Just s0 → HarvestSpot { sourceName: sourceName, nHarvs: nHarvs
                          , nMaxHarvs: nMaxHarvs', harvSpots: harvSpots' }
      where nMaxHarvs'   = nMaxHarvs - 1
            harvSpots'   = removeVal s0 harvSpots
            containerX   = RP.x containerPos
            containerY   = RP.y containerPos
  where containerPos = RO.pos container
containerInHarvestSpot ∷ RoomPosition → Array Spot → Maybe Spot
containerInHarvestSpot _   []        = Nothing
containerInHarvestSpot pos harvSpots = case uncons harvSpots of
  Nothing              → Nothing
  Just {head:h,tail:t} → if pos `posEqSpot` h then Just h else
                           containerInHarvestSpot pos t

-- | boolean equivalence of room position and spot
posEqSpot ∷ RoomPosition → Spot → Boolean
posEqSpot pos (Spot {spotType,spotX,spotY}) =
  ((RP.x pos) ≡ spotX) ∧ ((RP.y pos) ≡ spotY)

-- | boolean for the find' function, returns true if struct is of type
structIsType ∷ ∀ α. StructureType → Structure α → Boolean
structIsType stype struct = (structureType struct) ≡ stype

-- | returns true if creep is of type
creepIsType ∷ CreepType → F.Object CreepType → String → Creep → Boolean
creepIsType creepType creepTypes key _ = case (F.lookup key creepTypes) of
  Nothing → false
  Just v0 → v0 ≡ creepType

-- | boolean for the find' function, returns true if site is of type
siteIsType ∷ StructureType → ConstructionSite → Boolean
siteIsType stype site = (CS.structureType site) ≡ stype

-- | returns true if the creep is a harvesting type
iHarvest ∷ F.Object Role → String → Creep → Boolean
iHarvest roles key _ = case (F.lookup key roles) of
  Nothing → false
  Just v0 → case v0 of
              RoleHarvester            → true
              RoleCollier              → true
              RoleUpgrader             → true
              RoleBuilder              → true
              RoleWorker (JobRepair _) → true
              _                        → false

-- | returns array of roles given creep array
makeRoleArray ∷ String → F.Object Json → Role
makeRoleArray _ val = case (getField val "role") of
  Left  _  → RoleNULL
  Right r0 → r0

-- | returns array of creepTypes given creep array
makeCreepTypeArray ∷ String → F.Object Json → CreepType
makeCreepTypeArray _ val = case (getField val "typ") of
  Left  _  → CreepNULL
  Right t0 → t0

-- | finds an open source
findOpenSource ∷ Array HarvestSpot → Array Source → Maybe Source
findOpenSource _        []  = Nothing
findOpenSource harvData arr = case hData of
    Nothing → Nothing
    Just hd → index arr ind
    where ind   = findMostOpenHarvs (-100) (-100) 0 harvData
          arr'  = case (deleteAt ind arr) of
                    Nothing → []
                    Just a0 → a0
          hData = index harvData ind

findMostOpenHarvs ∷ Int → Int → Int → Array HarvestSpot → Int
findMostOpenHarvs n nInd ind []  = nInd
findMostOpenHarvs n nInd ind arr = case uncons arr of
  Nothing → nInd
  Just {head:(HarvestSpot {sourceName, nHarvs, nMaxHarvs, harvSpots}),tail:t} →
    if (nMaxHarvs - nHarvs) > n then
         findMostOpenHarvs (nMaxHarvs - nHarvs) ind  (ind + 1) t
    else findMostOpenHarvs n                    nInd (ind + 1) t

spotToPos ∷ String → Spot → RoomPosition
spotToPos room (Spot {spotType,spotX,spotY}) = RP.createRoomPosition spotX spotY room
-- TODO: this needs to find the tile type
posToSpot ∷ RoomPosition → Spot
posToSpot rp = Spot {spotType:SpotPlain,spotX:(RP.x rp),spotY:(RP.y rp)}

hasFreeSpace ∷ ∀ α. Structure α → Boolean
hasFreeSpace structure
  =  (isStorable structure (structureType structure))
  && (structureType structure) ≠ structure_container
  && (spawnStoreCapacity > 0)
  where spawnStoreCapacity = case (RO.storeMaybe structure) of
                               Nothing → 0
                               Just s0 → Store.getFreeCapacity' s0 resource_energy

-- | basic functino to find nearest object of type
findNearest ∷ ∀ α. Array (RoomObject α) → RoomPosition → Maybe (RoomObject α)
findNearest arr pos0 = index arr ind
  where ind    = findMin dists
        dists  = map (findDistance x0 y0) arr
        x0     = RP.x   pos0
        y0     = RP.y   pos0
findDistance ∷ ∀ α. Int → Int → RoomObject α → Int
findDistance x0 y0 obj = distance x0 y0 x1 y1
  where pos1 = RO.pos obj
        x1   = RP.x   pos1
        y1   = RP.y   pos1

