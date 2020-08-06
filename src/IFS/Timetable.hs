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
import           Data.List         ( nub )
import qualified Data.Map          as M
import           Data.Maybe        ( catMaybes )
import qualified Data.Set          as S

import           IFS.Types

--------------------------------------------------------------------------------

type Slots slot = S.Set slot

-- | `noOverlap` @vs@ ensures that no Just values in @vs@ are the same
noOverlap :: Eq slot => [Maybe slot] -> Bool
noOverlap vs = length (nub assigned) == length assigned
    where assigned = catMaybes vs

-- | `noConcurrentOverlap` @vs slots@ ensures that Just values in @vs@ never
-- occur in concurrent slots
noConcurrentOverlap :: Ord slot => [Maybe slot] -> [Slots slot] -> Bool
noConcurrentOverlap vs slots = all (<=1) $ map (S.size . S.intersection vs') slots
    where vs' = S.fromList $ catMaybes vs

-- | `flattenSlots` @slots@ flattens a list of slot sets to a single set
flattenSlots :: Ord slot => [Slots slot] -> (Slots slot)
flattenSlots = foldl S.union S.empty

-- | `calcDomains` @slots events availability@ creates the domain for each event
-- by finding all the slots where any member of the event is unavailable and
-- removing them from the list of slots
calcDomains :: (Eq user, Hashable user, Ord event, Ord slot)
            => [Slots slot]
            -> HM.HashMap event [user]
            -> HM.HashMap user (Slots slot)
            -> Domains event slot
calcDomains slots events availability =
    -- generate map of domains for every event
    flip (flip HM.foldlWithKey' M.empty) events $ \m event users ->
        -- add domain for this event - all slots where no one is busy
        flip (M.insert event) m $ S.difference (flattenSlots slots) $
            -- generate all slots where any member is unavailable
            foldl (\s u -> s `S.union` (availability HM.! u)) S.empty users

-- | `flipHashmap` @hm@ converts the hashmap of lists of type b with key a to
-- a hashmap index on values of b linked to lists of a
flipHashmap :: (Eq a, Hashable a, Eq b, Hashable b)
            => HM.HashMap a [b]
            -> HM.HashMap b [a]
flipHashmap hm = HM.fromListWith (++) $ concat $ flip HM.mapWithKey hm $
    \k vs -> map (\v -> (v, [k])) vs

-- | `calcConstraints` @slots events@ creates the constraints which stop the same
-- slot being used by 2 events, and the same user being assigned to 2 places at
-- once
calcConstraints :: (Eq user, Hashable user, Ord slot, Ord event, Hashable event)
                => [Slots slot]
                -> HM.HashMap event [user]
                -> Constraints event slot
calcConstraints slots events =
    let slots' = flattenSlots slots
        eventKeys= HM.keys events
    in
    -- prevent duplicate slot usage
    (S.fromList eventKeys, \a -> noOverlap [a M.!? i | i <- eventKeys])
    -- -- prevent the same user being allocated to multple places at the same time
    : map (\xs -> (S.fromList xs, \a -> noConcurrentOverlap [a M.!? i | i <- xs] slots))
        (filter ((>1) . length) $ HM.elems $ flipHashmap events)

-- | `toCSP` @slots events userAvailability@ converts the given data into a CSP
toCSP :: (Eq user, Hashable user, Ord event, Ord slot, Hashable event)
      => [Slots slot]
      -> HM.HashMap event [user]
      -> HM.HashMap user (Slots slot)
      -> CSP event slot
toCSP slots events availability = CSP {
    -- variables are the events
    cspVariables = S.fromList $ HM.keys events,
    -- domains are the slots the events may be assigned to
    cspDomains = calcDomains slots events availability,
    -- constraints prevent several events being assigned to the same slot and
    -- users being assigned to 2 places at once
    cspConstraints = calcConstraints slots events,
    -- iterate a maximum of 10 times the number of events before switching to random
    -- variable selection
    cspRandomCap = 10 * HM.size events,
    -- use default termination condition
    cspTermination = Nothing
}

--------------------------------------------------------------------------------
-- TEST DATA:                                                                 --
--------------------------------------------------------------------------------

slots :: [Slots Int]
slots = map (\x -> S.fromList [2*x + 1, 2*x + 2]) [0..3]

slots' :: [Slots Int]
slots' = [S.fromList [1..4], S.fromList [5..8]]

events :: HM.HashMap Int [String]
events = HM.fromList $ zip [1..8] $
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

usersAvail :: HM.HashMap String (Slots Int)
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

-- events :: HM.HashMap event [String]
-- events = HM.fromList $ zip [1..8] $ map (\i -> ["u" ++ show i, "v" ++ show (1 + (i `mod` 4))]) [1..8]

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


-- Processing this CSP will give an assignment of events to slots - variables represent events
-- and their values represent the slots
