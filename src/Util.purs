module Util where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index, head, zip, length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Screeps.Data
import Screeps.Room ( find )
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Structure ( structureType )
import Screeps.Store as Store
import Screeps.Const ( resource_energy, find_my_construction_sites)
import Maths (distance, findMin)
import Data (HarvestSpot(..), Spot(..))
import CG

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

-- will only return a source if it has capacity
findNearestOpenSource ∷ Array HarvestSpot → Array Source → RoomPosition → Maybe Source
-- if there was an error in reading harvestspots, just throw them away
--findNearestOpenSource _  arr pos0 = findNearest arr pos0
findNearestOpenSource harvData arr pos0 = index arr ind
  where ind    = findMin dists
        dists  = map (findSourceDistance x0 y0) (zip harvData arr)
        x0     = RP.x pos0
        y0     = RP.y pos0
findSourceDistance ∷ Int → Int → Tuple HarvestSpot Source → Int
findSourceDistance x0 y0 (Tuple spot source) = if avail then distance x2 y2 x0 y0
                                               -- TODO: figure out the optimal value
                                               --       for when we can begin doubling
                                               --       up on creeps
                                               else 1000000
  where pos1      = RO.pos source
        x1        = RP.x   pos1
        y1        = RP.y   pos1
        (HarvestSpot {sourceName, nHarvs, nMaxHarvs, harvSpots}) = spot
        x2     = case (head (harvSpots)) of
                   Nothing → 1000000
                   Just (Spot {spotType, spotX, spotY}) → spotX
        y2     = case (head (harvSpots)) of
                   Nothing → 1000000
                   Just (Spot {spotType, spotX, spotY}) → spotY


        avail  = spotAvailable spot
spotAvailable ∷ HarvestSpot → Boolean
spotAvailable (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots })
  = nHarvs < nMaxHarvs

setNHarvs ∷ Array HarvestSpot → Id Source → Spawn → CG Env Unit
setNHarvs harvs sourceId spawn = do
  let newHarvs = map (setNHarvsF sourceId) harvs
  setSpawnMem spawn "harvestSpots" newHarvs
setNHarvsF ∷ Id Source → HarvestSpot → HarvestSpot
setNHarvsF sourceId (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }) =
  HarvestSpot { sourceName: sourceName, nHarvs:    (nHarvs + n)
              , nMaxHarvs:  nMaxHarvs,  harvSpots: harvSpots }
  where n = if (sourceId ≡ sourceName) then 1 else 0
-- | remove a harvester from a spawn's memory
removeNHarvs ∷ Array HarvestSpot → Id Source → Spawn → CG Env Unit
removeNHarvs harvs sourceId spawn = do
  let newHarvs = map (removeNHarvsF sourceId) harvs
  setSpawnMem spawn "harvestSpots" newHarvs
removeNHarvsF ∷ Id Source → HarvestSpot → HarvestSpot
removeNHarvsF sourceId (HarvestSpot { sourceName, nHarvs, nMaxHarvs, harvSpots }) =
  HarvestSpot { sourceName: sourceName, nHarvs:    (nHarvs - n)
              , nMaxHarvs:  nMaxHarvs,  harvSpots: harvSpots }
  where n = if (sourceId ≡ sourceName) then 1 else 0

hasFreeSpace ∷ ∀ a. Structure a → Boolean
hasFreeSpace structure
  =  (isStorable structure (structureType structure))
  && (spawnStoreCapacity > 0)
  where spawnStoreCapacity = case (RO.storeMaybe structure) of
                               Nothing → 0
                               Just s0 → Store.getFreeCapacity' s0 resource_energy
isStorable ∷ ∀ a. Structure a → StructureType → Boolean
isStorable structure structure_spawn     = true
isStorable structure structure_tower     = true
isStorable structure structure_extension = true
isStorable _         _                   = false

-- | find constructions sites for a spawn
findCS ∷ ∀ α. Array Spawn → Array Int
findCS = map (\s → length (find (RO.room s) find_my_construction_sites))
