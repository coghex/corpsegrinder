module Spawn where

import UPrelude
import Effect.Class (liftEffect)
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, length, filter)
import Screeps.Data
import Screeps.Const (find_sources)
import Screeps.Game as Game
import Screeps.Room as Room
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.RoomTerrain as RT
import Screeps.Source as Source
import Foreign.Object as F
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

walkable ∷ Spot → Boolean
walkable (Spot {spotType: SpotPlain, spotX: _, spotY: _}) = true
walkable (Spot {spotType: SpotWall , spotX: _, spotY: _}) = false
walkable (Spot {spotType: SpotSwamp, spotX: _, spotY: _}) = true
walkable (Spot {spotType: SpotLava , spotX: _, spotY: _}) = false
