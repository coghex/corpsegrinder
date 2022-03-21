module Builder where

import UPrelude
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Array (length, uncons, take, index, head, tail, reverse)
import Foreign.Object as F
import Screeps.Data
import Screeps.Room as Room
import Screeps.Game as Game
import Screeps.Structure.Controller as Controller
import Screeps.RoomObject as RO
import Screeps.RoomPosition as RP
import Screeps.Const (find_construction_sites, find_my_structures, find_structures
                     , structure_container, structure_extension, ok)
import CorpseGrinder
import Spawn
import Util (structIsType, siteIsType)
import Maths (shuffleList)
import Data (HarvestSpot(..), Spot(..), LogLevel(..))

buildRoom ∷ Spwn Unit
buildRoom = do
  s1 ← asks (_.spawn)
  let rcl         = Controller.level controller1
      controller1 = Room.controller  room1
      room1       = RO.room          s1
  case rcl of
    1 → do
      let sites  = Room.find (RO.room s1) find_construction_sites
          nSites = length sites
      -- at level one, we only want one construction site at a time
      if nSites ≡ 0 then do
        game ← lift $ asks (_.game)
        let creeps = Game.creeps game
            nCreeps = F.size creeps
            -- containers built and blueprinted
            nContainers = nBuildConts
            nBuildConts = length $ Room.find' (RO.room s1)
                                     find_my_structures
                                     $ structIsType structure_container
        if nCreeps > 2 then
        -- only builds one container for the first rcl
          if nContainers < 2 then
            buildContainer nContainers
          else pure unit
        else pure unit
      else pure unit
    2 → do
      game ← lift $ asks (_.game)
      let sites  = Room.find (RO.room s1) find_construction_sites
          nSites = length sites
      if nSites < 1 then do
        let creeps = Game.creeps game
            nContainers   = nBuildConts + nContsSites
            nContsSites   = length $ Room.find' (RO.room s1)
                                       find_construction_sites
                                       $ siteIsType structure_container
            nBuildConts   = length $ Room.find' (RO.room s1)
                                       find_structures
                                       $ structIsType structure_container
            nExtensions   = nExtsSites + nBuildExts
            nExtsSites    = length $ Room.find' (RO.room s1)
                                       find_construction_sites
                                       $ siteIsType structure_extension
            nBuildExts    = length $ Room.find' (RO.room s1)
                                     find_structures
                                     $ structIsType structure_extension
            nCreeps       = F.size creeps
        if nCreeps > 2 then do
        -- finish out building containers, then we can use static harvesting
          harvestSpots' ← getSpawnMem' "harvestSpots"
          let maxContainers = case harvestSpots' of
                                Nothing → 0
                                Just h0 → length (h0 ∷ Array HarvestSpot)
          -- else if, or else it might overbuild
          if nContainers < maxContainers then
            buildContainer nContainers
          else if nExtensions < 5 then
            buildExtension nExtensions s1
          else pure unit
        else pure unit
      else pure unit
    _ → pure unit

buildExtension ∷ Int → Spawn → Spwn Unit
buildExtension nExtensions spawn = do
  let spawnPos      = RO.pos spawn
      x             = RP.x spawnPos
      y             = RP.y spawnPos
      possibleSites = [ TargetPt (x + 1) y
                      , TargetPt (x + 1) (y + 1)
                      , TargetPt (x + 1) (y - 1)
                      , TargetPt x       (y + 1)
                      , TargetPt x       (y - 1)
                      , TargetPt (x - 1) y
                      , TargetPt (x - 1) (y + 1)
                      , TargetPt (x - 1) (y - 1) ]
      pos = possibleSites `index` (nExtensions `mod` 9)
  case pos of
    Nothing → log'' LogError "cant create site"
    Just p0 → buildExtensionF p0 possibleSites
buildExtensionF ∷ ∀ α. TargetPosition α
  → Array (TargetPosition α) → Spwn Unit
buildExtensionF _   []    = log'' LogWarn "no possible sites for extensions"
buildExtensionF pos sites = do
  ret ← createConstructionSite' pos structure_extension
  if ret then pure unit else do
    case uncons sites of
      Nothing               → log'' LogError "fuck"
      Just {head:s,tail:ss} → buildExtensionF s ss

buildContainer ∷ Int → Spwn Unit
buildContainer nContainers = do
  harvestSpots' ← getSpawnMem' "harvestSpots"
  case harvestSpots' of
    Nothing → log'' LogError "buildContainer: no harvest spots"
    Just harvestSpots → do
        let possibleSites = findContainerSites harvestSpots
            -- max 5 containers at every level
            sites         = take 5 $ shuffleList possibleSites
            pos           = sites `index` nContainers
        case (sites `index` nContainers) of
          Nothing  → log'' LogError "cant create container"
          Just pos → buildContainerF pos $ reverse
                                     $ shuffleList possibleSites
-- | some construction sites return errors for various reasons, if
--   may conflict wit other containers but it will move on if so
--   that is the case we try and look for other options, these
buildContainerF ∷ ∀ α. TargetPosition α
  → Array (TargetPosition α) → Spwn Unit
buildContainerF _   []    = log'' LogWarn "no possible sites for containers"
buildContainerF pos sites = do
  ret ← createConstructionSite' pos structure_container
  if ret then pure unit else do
    case uncons sites of
      Nothing               → log'' LogError "fuck"
      Just {head:s,tail:ss} → buildContainerF s ss

findContainerSites ∷ ∀ α. Array HarvestSpot → Array (TargetPosition α)
findContainerSites spots = expandSpots spotArrays
  where spotArrays = map findContainerSite spots
findContainerSite ∷ ∀ α. HarvestSpot → Array Spot
findContainerSite (HarvestSpot {sourceName:_,nHarvs:_,nMaxHarvs:_,harvSpots:hs}) = hs
expandSpots ∷ ∀ α. Array (Array Spot) → Array (TargetPosition α)
expandSpots []    = []
expandSpots spots = sites <> expandSpots spots'
  where spots' = case uncons spots of
                   Just {head: _, tail: ss} → ss
                   Nothing                  → []
        sites  = case uncons spots of
                   Just {head: s, tail: _}  → findSites s
                   Nothing                  → []
findSites ∷ ∀ α. Array Spot → Array (TargetPosition α)
findSites spots = map spotPosition spots
spotPosition ∷ ∀ α. Spot → TargetPosition α
spotPosition (Spot {spotType:_, spotX:x, spotY:y}) = TargetPt x y
