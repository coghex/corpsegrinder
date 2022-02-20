module Processor where

import UPrelude
import Effect (Effect)
import Effect.Console (log)
import Data.Int ( quot )
import Data.Array (index, uncons, filter)
import Data.Argonaut.Core ( Json )
import Data.Argonaut.Encode ( encodeJson )
import Data.Argonaut.Decode ( decodeJson )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Memory as Memory
import Screeps.Structure.Spawn as Spawn
import Screeps.Store as Store
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Data

-- | creeps change their roles here
processCreeps ∷ GameGlobal → MemoryGlobal → Effect Unit
processCreeps game memory = do
  creeps' ← Memory.get memory "creeps"
  let creeps = case creeps' of
                 Left err → F.empty
                 Right c0 → c0
      utl0 = F.fold addUpUtils 0 creeps
      newCreeps = F.mapWithKey (processCreep utl0 roleScores roleList) creeps
      roleScores = map (calcRoleScore creeps) roleList
  Memory.set memory "creeps"  newCreeps
  Memory.set memory "utility" utl0
processCreep ∷ Int → Array Int → Array Role → String
  → F.Object Json → F.Object Json
processCreep utl0 scores roles key creep0 = creep2
  where creep2  = F.update (\_ → newUtl)  "utility" creep1
        creep1  = F.update (\_ → newRole) "role"    creep0
        newUtl  = fromMaybeToMaybeJson newUtl'
        newUtl' = scores `index` ind
        ind     = findInd roles alt 0
        alt     = bestRole scores roles 0 RoleNULL
        newRole = Just $ encodeJson alt

bestRole ∷ Array Int → Array Role → Int → Role → Role
bestRole []     _     _     role = role
bestRole _      []    _     role = role
bestRole scores roles score role = if (score' > score) then bestRole scores' roles' score' role'
   else bestRole scores' roles' score role
   where roles'  = case uncons roles of
                     Just {head: _, tail: rs} → rs
                     Nothing → []
         role'   = case uncons roles of
                     Just {head: r, tail: _}  → r
                     Nothing → RoleNULL
         scores' = case uncons scores of
                     Just {head: _, tail: ss} → ss
                     Nothing → []
         score'  = case uncons scores of
                     Just {head: s, tail: _}  → s
                     Nothing → 0

-- | finds the score for a given role for all the creeps at once
calcRoleScore ∷ F.Object (F.Object Json) → Role → Int
calcRoleScore creeps RoleNULL      = 0
calcRoleScore creeps RoleIdle      = 1
calcRoleScore creeps RoleBuilder   = score
  where score    = 800 `quot` (builders + 1)
        builders = numberOfRole RoleBuilder creeps
calcRoleScore creeps RoleHarvester = score
  where score = 1000 `quot` (harvs + 1)
        harvs = numberOfRole RoleHarvester creeps

numberOfRole ∷ Role → F.Object (F.Object Json) → Int
numberOfRole role creeps = F.size $ F.filter roleFilter creeps

roleFilter ∷ F.Object Json → Boolean
roleFilter creep = false

-- | we need to always return something or foreign.update will delete things
fromMaybeToMaybeJson ∷ Maybe Int → Maybe Json
fromMaybeToMaybeJson (Nothing) = Just $ encodeJson (-1)
fromMaybeToMaybeJson (Just v)  = Just $ encodeJson (v)

-- | TODO: be a good programmer and make this Array a → a → Int → Int
findInd ∷ Array Role → Role → Int → Int
findInd []    _   n = n
findInd array val n =
  if (val == val') then n else findInd array' val (n + 1)
  where val'   = case uncons array of
                   Just {head: a, tail: _}  → a
                   Nothing                  → RoleNULL
        array' = case uncons array of
                   Just {head: _, tail: al} → al
                   Nothing                  → []

addUpUtils ∷ Int → String → F.Object Json → Int
addUpUtils n key val = case (F.lookup "utility" val) of
  Nothing → n
  Just v0 → case (decodeJson v0) of
    Left err → n
    Right n0 → n + n0
