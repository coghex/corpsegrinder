module Processor where

import UPrelude
import Control.Monad.Reader (asks)
import Data.Int ( quot, round, toNumber )
import Data.Array (index, uncons, filter, foldr)
import Data.Argonaut.Core ( Json )
import Data.Argonaut.Encode ( encodeJson )
import Data.Argonaut.Decode ( decodeJson )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Screeps.Data
import Foreign.Object as F
import Screeps.Game as Game
import Screeps.Store as Store
import Screeps.Const (resource_energy, pWork, pMove, pCarry)
import Util (findCS)
import Spawn (energyCapacity, energyFreeSpace)
import Data
import CG

-- | creeps change their roles here
processCreeps ∷ CG Env Unit
processCreeps = do
  game ← asks (_.game)
  creeps' ← getMemField "creeps"
  let creeps = case creeps' of
                 Nothing → F.empty
                 Just c0 → c0
      utl0 = F.fold addUpUtils 0 creeps
      newCreeps = F.mapWithKey (processCreep
                    utl0 numCS (energyNeed)
                    roleList' creeps)
                    creeps
      -- TODO: this currently retreives global value, it should be per creep
      --       since creeps will eventually need to be all over the map
      numCS = foldr (+) 0 $ findCS $ F.values spawns
      spawns    = Game.spawns game
      roleList' = map setArgs roleList
      -- plus one is solely to avoid hopefully rare case where capacity is 0
      energyNeed  = round $ 100.0 * energyNeed'
      energyNeed' = (toNumber (energyFreeSpace spawns))
                  / (toNumber $ (energyCapacity spawns) + 1)
      setArgs (RoleBuilder _) = RoleBuilder numCS
      setArgs roleargument    = roleargument
  setMemField "creeps"  newCreeps
  setMemField "utility" utl0
processCreep ∷ Int → Int → Int → Array Role → F.Object (F.Object Json)
  → String → F.Object Json → F.Object Json
processCreep utl0 numCS energyNeed roles creeps key creep0 = creep3
  where creep3  = F.update (\_ → newUtl)  "utility"  creep2
        creep2  = F.update (\_ → newRole) "role"     creep1
        creep1  = switchRole creep0 alt
        -- TODO: unset target here, or maybe not, doesnt really matter
        --       if they are all dying all the time
        newUtl  = fromMaybeToMaybeJson newUtl'
        newUtl' = scores `index` ind
        ind     = findInd roles alt 0
        alt     = bestRole scores roles 0 RoleNULL
        newRole = Just $ encodeJson alt
        scores  = map (calcRoleScore energyNeed creeps role0) roles
        role0   = case getField' creep0 "role" of
                    Nothing → RoleNULL
                    Just r0 → r0

-- | helps translate a creep to a new role
--   , usually will fail so it doesnt error
switchRole ∷ F.Object Json → Role → F.Object Json
switchRole creep0 newrole = 
 case F.lookup "role" creep0 of
   Nothing → creep0
   Just r0 → case decodeJson r0 of
                 Left  _  → creep0
                 Right r1 → switchRoleF creep0 r1 newrole
switchRoleF ∷ F.Object Json → Role → Role → F.Object Json
switchRoleF creep0 role0 newrole = case role0 of
  RoleUpgrader → case newrole of
    (RoleBuilder _) → creep2 
                        where creep1 = F.delete "upgrading" creep0
                              creep2 = F.update (\_ → Just $ encodeJson v)
                                                 "building" creep1
                              v = case (getField' creep0 "upgrading") of
                                    Nothing → false
                                    Just v0 → v0
    _               → creep0
  RoleBuilder _ → case newrole of
    RoleUpgrader    → creep2
                        where creep1 = F.delete "building"   creep0
                              creep2 = F.update (\_ → Just $ encodeJson v)
                                                 "upgrading" creep1
                              v = case (getField' creep0 "building") of
                                    Nothing → false
                                    Just v0 → v0
    _               → creep0
  _            → creep0

-- | returns the best possible role given game state
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
calcRoleScore ∷ Int → F.Object (F.Object Json) → Role → Role → Int
calcRoleScore _          _      _     RoleNULL        = 0
calcRoleScore _          _      _     RoleIdle        = 1
calcRoleScore energyNeed creeps role0 RoleUpgrader    = score
  where score     = 1000 `quot` (upgraders + 1)
        upgraders = iDoThat + numberOfRole RoleUpgrader creeps
        iDoThat   = case role0 of
                      RoleUpgrader → -1
                      _            → 0
calcRoleScore energyNeed creeps role0 (RoleBuilder n) = score
  where score    = (2*n) * (400 `quot` (builders + 1))
        builders = iDoThat + numberOfRole (RoleBuilder n) creeps
        iDoThat  = case role0 of
                     RoleBuilder _ → -1
                     _             → 0
calcRoleScore energyNeed creeps role0 RoleHarvester = score
  where score   = (10*energyNeed + 500) `quot` ((10*harvs) + 1)

        harvs   = iDoThat + numberOfRole RoleHarvester creeps
        iDoThat = case role0 of
                    RoleHarvester → -1
                    _             → 0

numberOfRole ∷ Role → F.Object (F.Object Json) → Int
numberOfRole role creeps = F.size $ F.filter (roleFilter role) creeps

roleFilter ∷ Role → F.Object Json → Boolean
roleFilter role creep = case (F.lookup "role" creep) of
  Nothing → false
  Just r0 → case decodeJson r0 of
    Left  _  → false
    Right r0 → r0 ≡ role

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
    Left  _  → n
    Right n0 → n + n0
