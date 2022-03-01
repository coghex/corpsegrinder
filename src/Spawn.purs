module Spawn where

import UPrelude
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, length, filter, head)
import Screeps.Data
import Screeps.Const (find_sources)
import Screeps.Game as Game
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.RoomTerrain as RT
import Screeps.Structure (structureType)
import Screeps.Const (find_my_structures, resource_energy)
import Screeps.Source as Source
import Screeps.Store as Store
import Foreign.Object as F
import Util (isStorable)
import Data
import CG

initSpawn ∷ CG Env Unit
initSpawn = do
  game ← asks (_.game)
  let spawnsList = Game.spawns game
  -- TODO: generalize the following
      spawn1     = F.lookup "Spawn1" spawnsList
  case spawn1 of
    Nothing → pure unit
    Just s1 → do
      let r            = RO.room s1
          harvestSpots = findAllHarvestSpots r sources
          sources      = Room.find r find_sources
      setSpawnMem s1 "harvestSpots" harvestSpots

findAllHarvestSpots ∷ Room → Array Source → Array HarvestSpot
findAllHarvestSpots _    []      = []
findAllHarvestSpots room sources = res <> (findAllHarvestSpots room sources')
  where sources'     = case uncons sources of
                         Just {head: _, tail: ss} → ss
                         Nothing                  → []
        res          = case uncons sources of
                         Just {head: s, tail: _}  → [findHarvestSpots room s]
                         Nothing                  → []
findHarvestSpots ∷ Room → Source → HarvestSpot
findHarvestSpots room source
  = HarvestSpot { sourceName: sname
                , nHarvs:     0
                , nMaxHarvs:  nmax
                , harvSpots:  spots1 }
  where sname   = Source.id source
        nmax    = length spots1
        terrain = Room.getTerrain room
        spots1  = filter walkable spots0
        spots0  = spotN <> spotS <> spotE <> spotW
                    <> spotNW <> spotNE <> spotSW <> spotSE
        sPos    = RO.pos      source
        x       = RP.x        sPos
        y       = RP.y        sPos
        spotN   = unMaybeSpot terrain x       (y + 1)
        spotS   = unMaybeSpot terrain x       (y - 1)
        spotE   = unMaybeSpot terrain (x + 1) y
        spotW   = unMaybeSpot terrain (x - 1) y
        spotNW  = unMaybeSpot terrain (x - 1) (y + 1)
        spotNE  = unMaybeSpot terrain (x + 1) (y + 1)
        spotSW  = unMaybeSpot terrain (x - 1) (y - 1)
        spotSE  = unMaybeSpot terrain (x + 1) (y - 1)
unMaybeSpot ∷ RoomTerrain → Int → Int → Array Spot
unMaybeSpot rt x y = case (RT.get rt x y) of
  Nothing → [Spot { spotType: SpotPlain
                  , spotX:    x
                  , spotY:    y}]
  Just s0 → tType s0
  where tType 0 = [Spot { spotType: SpotPlain
                        , spotX: x
                        , spotY: y }]
        tType 1 = [Spot { spotType: SpotWall
                        , spotX:    x
                        , spotY:    y}]
        tType 2 = [Spot { spotType: SpotSwamp
                        , spotX:    x
                        , spotY:    y}]
        tType _ = [Spot { spotType: SpotLava
                        , spotX:    x
                        , spotY:    y}]

-- | not every spot is walkable
walkable ∷ Spot → Boolean
walkable (Spot {spotType: SpotPlain, spotX: _, spotY: _}) = true
walkable (Spot {spotType: SpotWall , spotX: _, spotY: _}) = false
walkable (Spot {spotType: SpotSwamp, spotX: _, spotY: _}) = true
walkable (Spot {spotType: SpotLava , spotX: _, spotY: _}) = false

-- | adds up the total capacity of all energy stores
energyCapacity ∷ F.Object Spawn → Int
energyCapacity spawns =
  case spawn1 of
    Nothing → 0
    Just s1 → energyCapacityF s1
  where spawn1 = head $ F.toArrayWithKey (\_ s → s) spawns
energyCapacityF ∷ Spawn → Int
energyCapacityF spawn = addUpCapacities stores
  where stores = Room.find' room find_my_structures
                 (\s → isStorable s (structureType s))
        room   = RO.room spawn
addUpCapacities ∷ ∀ α. Array (Structure α) → Int
addUpCapacities []  = 0
addUpCapacities arr = str0 + addUpCapacities arr'
  where arr' = case uncons arr of
                 Just {head:_,tail:a} → a
                 Nothing              → []
        str0 = case uncons arr of
                 Just {head:s,tail:_} → case (RO.storeMaybe s) of
                                          Nothing → 0
                                          Just s0 → Store.getCapacity' s0 resource_energy
                 Nothing              → 0

-- | adds up the total free space for energy of
--   all stores of structs in room of spawn
energyFreeSpace ∷ F.Object Spawn → Int
energyFreeSpace spawns =
  case spawn1 of
    Nothing → 0
    Just s1 → energyFreeSpaceF s1
  where spawn1 = head $ F.toArrayWithKey (\_ s → s) spawns
energyFreeSpaceF ∷ Spawn → Int
energyFreeSpaceF spawn = addUpFreeSpace stores
  where stores = Room.find' room find_my_structures
                 (\s → isStorable s (structureType s))
        room   = RO.room spawn
addUpFreeSpace ∷ ∀ α. Array (Structure α) → Int
addUpFreeSpace []  = 0
addUpFreeSpace arr = str0 + addUpFreeSpace arr'
  where arr' = case uncons arr of
                 Just {head:_,tail:a} → a
                 Nothing              → []
        str0 = case uncons arr of
                 Just {head:s,tail:_} → case (RO.storeMaybe s) of
                                          Nothing → 0
                                          Just s0 → Store.getFreeCapacity' s0
                                                      resource_energy
                 Nothing              → 0

-- | adds up the total used space for energy of
--   all stores of structs in room of spawn
energyUsedSpace ∷ F.Object Spawn → Int
energyUsedSpace spawns =
  case spawn1 of
    Nothing → 0
    Just s1 → energyUsedSpaceF s1
  where spawn1 = head $ F.toArrayWithKey (\_ s → s) spawns
energyUsedSpaceF ∷ Spawn → Int
energyUsedSpaceF spawn = addUpUsedSpace stores
  where stores = Room.find' room find_my_structures
                 (\s → isStorable s (structureType s))
        room   = RO.room spawn
addUpUsedSpace ∷ ∀ α. Array (Structure α) → Int
addUpUsedSpace []  = 0
addUpUsedSpace arr = str0 + addUpUsedSpace arr'
  where arr' = case uncons arr of
                 Just {head:_,tail:a} → a
                 Nothing              → []
        str0 = case uncons arr of
                 Just {head:s,tail:_} → case (RO.storeMaybe s) of
                                          Nothing → 0
                                          Just s0 → Store.getUsedCapacity' s0
                                                      resource_energy
                 Nothing              → 0


