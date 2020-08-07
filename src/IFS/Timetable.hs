--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module IFS.Timetable where

--------------------------------------------------------------------------------

import           Data.Hashable
import qualified Data.HashMap.Lazy           as HM
import           Data.IntervalMap.FingerTree
import qualified Data.IntMap                 as M
import qualified Data.IntSet                 as S
import           Data.List                   ( nub )
import           Data.Maybe                  ( catMaybes )
import           Data.Time

import           IFS.Types

--------------------------------------------------------------------------------

type Slots = S.IntSet
type Slot = Int
type Event = Int
type User = Int

-- | `noOverlap` @vs@ ensures that no Just values in @vs@ are the same
noOverlap :: [Maybe Slot] -> Bool
noOverlap vs = length (nub assigned) == length assigned
    where assigned = catMaybes vs

-- | `noConcurrentOverlap` @vs slots@ ensures that the assigned values (the Just
-- values) in @vs@ do not overlap. @slots@ is used to fetch the interval for
-- each slot
noConcurrentOverlap :: [Maybe Slot] -> M.IntMap (Interval UTCTime) -> Bool
noConcurrentOverlap vs slots = snd $ foldl f (empty, True) vs'
    where vs' = catMaybes vs
          f (im, False) _ = (im, False)
          f (im, True) s = let interval = (slots M.! s) in
                           if null $ interval `intersections` im
                           then (insert interval () im, True)
                           else (im, False)

-- | `calcDomains` @slots events availability@ creates the domain for each event
-- by finding all the slots where any member of the event is unavailable and
-- removing them from the list of slots
calcDomains :: (Eq user, Hashable user)
            => Slots
            -> HM.HashMap Event [user]
            -> HM.HashMap user Slots
            -> Domains
calcDomains slots events availability =
    -- generate map of domains for every event
    flip (flip HM.foldlWithKey' M.empty) events $ \m event users ->
        -- add domain for this event - all slots where no one is busy
        flip (M.insert event) m $ S.difference slots $
            -- generate all slots where any member is unavailable
            foldl (\s u -> s `S.union` (availability HM.! u)) S.empty users

-- | `flipHashmap` @hm@ converts the hashmap of lists of type b with key a to
-- a hashmap index on values of b linked to lists of a
flipHashmap :: (Eq a, Hashable a, Eq b, Hashable b)
            => HM.HashMap a [b]
            -> HM.HashMap b [a]
flipHashmap hm = HM.fromListWith (++) $ concat $ flip HM.mapWithKey hm $
    \k vs -> map (\v -> (v, [k])) vs

-- | `calcConstraints` @slots slotMap events@ creates the constraints which stop
-- the same slot being used by 2 events, and the same user being assigned to 2
-- places at once
calcConstraints :: (Eq user, Hashable user)
                => Slots
                -> M.IntMap (Interval UTCTime)
                -> HM.HashMap Event [user]
                -> Constraints
calcConstraints slots slotMap events =
    let eventKeys= HM.keys events in
    -- prevent duplicate slot usage
    (S.fromList eventKeys, \a -> noOverlap [a M.!? i | i <- eventKeys])
    -- prevent the same user being allocated to multple places at the same time
    : map (\xs -> (S.fromList xs, \a -> noConcurrentOverlap [a M.!? i | i <- xs] slotMap))
        (filter ((>1) . length) $ HM.elems $ flipHashmap events)

-- | `toCSP` @slots events userAvailability@ converts the given data into a CSP
toCSP :: (Eq user, Hashable user)
      => M.IntMap (Interval UTCTime)
      -> HM.HashMap Event [user]
      -> HM.HashMap user Slots
      -> CSP
toCSP slotMap events availability = let slots = M.keysSet slotMap in CSP {
    -- variables are the events
    cspVariables = S.fromList $ HM.keys events,
    -- domains are the slots the events may be assigned to
    cspDomains = calcDomains slots events availability,
    -- constraints prevent several events being assigned to the same slot and
    -- users being assigned to 2 places at once
    cspConstraints = calcConstraints slots slotMap events,
    -- iterate a maximum of 10 times the number of events before switching to random
    -- variable selection
    cspRandomCap = 10 * HM.size events,
    -- use default termination condition
    cspTermination = Nothing
}

--------------------------------------------------------------------------------
