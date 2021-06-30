--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE file in  --
-- the root directory of this source tree.                                    --
--------------------------------------------------------------------------------

import           Criterion.Main

import           Control.Monad

import qualified Data.HashMap.Lazy           as HM
import           Data.IntervalMap.FingerTree
import qualified Data.IntMap                 as IM
import qualified Data.IntSet                 as IS
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Data.IFS.Algorithm
import           Data.IFS.Timetable
import           Data.IFS.Types

--------------------------------------------------------------------------------

-- | A value representing 9am
am9 :: UTCTime
am9 = posixSecondsToUTCTime 1625043600

-- | A convenient operator for adding seconds to `UTCTime`
(+.) :: UTCTime -> NominalDiffTime -> UTCTime
(+.) = flip addUTCTime

-- | Possible slot set (max-assignment 8):
-- 4 sets of 2 overlapping slots (like 4 time slots with 2 rooms each)
solvableSlots :: IM.IntMap (Interval UTCTime)
solvableSlots = IM.fromList [ (1, Interval am9 (am9 +. 3600))
                            , (2, Interval am9 (am9 +. 3600))
                            , (3, Interval (am9 +. 3600) (am9 +. (2*3600)))
                            , (4, Interval (am9 +. 3600) (am9 +. (2*3600)))
                            , (5, Interval (am9 +. (2*3600)) (am9 +. (3*3600)))
                            , (6, Interval (am9 +. (2*3600)) (am9 +. (3*3600)))
                            , (7, Interval (am9 +. (3*3600)) (am9 +. (4*3600)))
                            , (8, Interval (am9 +. (3*3600)) (am9 +. (4*3600)))
                            ]

-- | Impossible slot set (max-assignment 6):
-- 2 sets of 4 overlapping slots (like 2 time slots with 4 rooms each)
unsolvableSlots :: IM.IntMap (Interval UTCTime)
unsolvableSlots = IM.fromList [ (1, Interval am9 (am9 +. 3600))
                              , (2, Interval am9 (am9 +. 3600))
                              , (3, Interval am9 (am9 +. 3600))
                              , (4, Interval am9 (am9 +. 3600))
                              , (5, Interval (am9 +. 3600) (am9 +. (2*3600)))
                              , (6, Interval (am9 +. 3600) (am9 +. (2*3600)))
                              , (7, Interval (am9 +. 3600) (am9 +. (2*3600)))
                              , (8, Interval (am9 +. 3600) (am9 +. (2*3600)))
                              ]

-- | A HashMap of who should be at certain events
events :: HM.HashMap Event [String]
events = HM.fromList $ zip [1..8]
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

-- | A HashMap of when each person is available
usersAvail :: HM.HashMap String Slots
usersAvail = HM.map IS.fromList $ HM.fromList
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

--------------------------------------------------------------------------------

-- | A CSP made from the solvable slots
cspSolvable :: CSP Solution
cspSolvable = toCSP solvableSlots events usersAvail defaultTermination

-- | A CSP made from the unsolvable slots
cspUnsolvable :: CSP Solution
cspUnsolvable = toCSP unsolvableSlots events usersAvail defaultTermination

-- | `countExpectedLength` @expected solutions@ counts the number of solutions
-- that have @expected@ variables assigned
countExpectedLength :: Int -> [Solution] -> Int
countExpectedLength n = length . filter ((==n) . IM.size . fromSolution)

main :: IO ()
main = do
    -- count how many times /1000 the result is the best length
    resultsSolveable <- replicateM 1000 $ ifs cspSolvable IM.empty
    putStrLn $ "Solvable Best: " ++ show (countExpectedLength 8 resultsSolveable)
    resultsUnsolveable <- replicateM 1000 $ ifs cspUnsolvable IM.empty
    putStrLn $ "Unsolvable Best: " ++ show (countExpectedLength 6 resultsUnsolveable)

    -- benchmark solvable and unsolvable CSPs
    defaultMain
        [
            bgroup "Basic IFS Tests"
            [
                bench "solvable" $
                    nfIO (ifs cspSolvable IM.empty),
                bench "unsolvable" $
                    nfIO (ifs cspUnsolvable IM.empty)
            ]
        ]

--------------------------------------------------------------------------------
