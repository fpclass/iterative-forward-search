--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module IFS.Timetable where

--------------------------------------------------------------------------------

import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.IntMap       as M
import           Data.List         ( nub )
import           Data.Maybe        ( catMaybes )
import qualified Data.Set          as S

import           IFS.Types

--------------------------------------------------------------------------------

type Slots = S.Set Int
type Group = Int

-- | `noOverlap` @vs@ ensures that no Just values in @vs@ are the same
noOverlap :: [Maybe Int] -> Bool
noOverlap vs = length (nub assigned) == length assigned
    where assigned = catMaybes vs

-- | `noConcurrentOverlap` @vs slots@ ensures that Just values in @vs@ never occur
-- in concurrent slots
noConcurrentOverlap :: [Maybe Int] -> [Slots] -> Bool
noConcurrentOverlap vs slots = all (<=1) $ map (S.size . S.intersection vs') slots
    where vs' = S.fromList $ catMaybes vs

-- | `flattenSlots` @slots@ flattens a list of slot sets to a single set
flattenSlots :: [Slots] -> Slots
flattenSlots = foldl S.union S.empty

-- | `calcDomains` @slots groups availability@ creates the domain for each group
-- by finding all the slots where any member of the group is unavailable and
-- removing them from the list of slots
calcDomains :: (Eq user, Hashable user)
            => [Slots]
            -> HM.HashMap Group [user]
            -> HM.HashMap user Slots
            -> Domains
calcDomains slots groups availability =
    -- generate map of domains for every group
    flip (flip HM.foldlWithKey' M.empty) groups $ \m group users ->
        -- add domain for this group - all slots where no one is busy
        flip (M.insert group) m $ S.difference (flattenSlots slots) $
            -- generate all slots where any member is unavailable
            foldl (\s u -> s `S.union` (availability HM.! u)) S.empty users

-- | `flipHashmap` @hm@ converts the hashmap of lists of type b with key a to
-- a hashmap index on values of b linked to lists of a
flipHashmap :: (Eq a, Hashable a, Eq b, Hashable b)
            => HM.HashMap a [b]
            -> HM.HashMap b [a]
flipHashmap hm = HM.fromListWith (++) $ concat $ flip HM.mapWithKey hm $
    \k vs -> map (\v -> (v, [k])) vs

-- | `calcConstraints` @slots groups@ creates the constraints which stop the same
-- slot being used by 2 groups, and the same user being assigned to 2 places at
-- once
calcConstraints :: (Eq user, Hashable user)
                => [Slots]
                -> HM.HashMap Group [user]
                -> Constraints
calcConstraints slots groups = let slots' = flattenSlots slots in
    -- prevent duplicate slot usage
    (slots', \a -> noOverlap [a M.!? i | i <- S.toList slots'])
    -- prevent the same user being allocated to multple places at the same time
    : map (\xs -> (S.fromList xs, \a -> noConcurrentOverlap [a M.!? i | i <- xs] slots))
        (filter ((>1) . length) $ HM.elems $ flipHashmap groups)

-- | `toCSP` @slots groups userAvailability@ converts the given data into a CSP
toCSP :: (Eq user, Hashable user)
      => [Slots]
      -> HM.HashMap Group [user]
      -> HM.HashMap user Slots
      -> CSP
toCSP slots groups availability = CSP {
    -- variables are the groups
    cspVariables = S.fromList $ HM.keys groups,
    -- domains are the slots the groups may be assigned to
    cspDomains = calcDomains slots groups availability,
    -- constraints prevent several groups being assigned to the same slot and
    -- users being assigned to 2 places at once
    cspConstraints = calcConstraints slots groups,
    -- iterate a maximum of 10 times the number of groups before switching to random
    -- variable selection
    cspRandomCap = 10 * HM.size groups,
    -- use default termination condition
    cspTermination = Nothing
}

--------------------------------------------------------------------------------
-- TEST DATA:                                                                 --
--------------------------------------------------------------------------------

slots :: [Slots]
slots = map (\x -> S.fromList [2*x + 1, 2*x + 2]) [0..3]

slots' :: [Slots]
slots' = [S.fromList [1..8]]

groups :: HM.HashMap Group [String]
groups = HM.fromList $ zip [1..8] $
    [
        ["v", "m"],
        ["r", "pa"],
        ["v", "t"],
        ["v", "pe"],
        ["a", "j"],
        ["m", "r"],
        ["pa", "s"],
        ["pe", "v"]
    ]

usersAvail :: HM.HashMap String Slots
usersAvail = HM.map S.fromList $ HM.fromList
    [
        ("v", []),
        ("m", []),
        ("r", []),
        ("pa", [2, 3, 6, 7]),
        ("t", []),
        ("pe", [1..5]),
        ("a", []),
        ("j", 2:[4..8]),
        ("s", [1..3])
    ]

-- slots :: [Slots]
-- slots = [S.fromList [1..4], S.fromList [5..8]]

-- groups :: HM.HashMap Group [String]
-- groups = HM.fromList $ zip [1..8] $ map (\i -> ["u" ++ show i, "v" ++ show (1 + (i `mod` 4))]) [1..8]

-- usersAvail :: HM.HashMap String Slots
-- usersAvail = HM.map S.fromList $ HM.fromList
--     [
--         ("u1", []),
--         ("u2", []),
--         ("u3", []),
--         ("u4", []),
--         ("u5", []),
--         ("u6", []),
--         ("u7", []),
--         ("u8", []),
--         ("v1", []),
--         ("v2", []),
--         ("v3", []),
--         ("v4", [])
--     ]


-- Processing this CSP will give an assignment of groups to slots - variables represent groups
-- and their values represent the slots
