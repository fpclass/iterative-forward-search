--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE file in  --
-- the root directory of this source tree.                                    --
--------------------------------------------------------------------------------

import           Criterion.Main

import           Control.Monad

import qualified Data.Map           as M
import qualified Data.Set           as S

import           Data.IFS.Algorithm
import           Data.IFS.Timetable
import           Data.IFS.Types

--------------------------------------------------------------------------------

slots :: [Slots]
slots = map (\x -> S.fromList [2*x + 1, 2*x + 2]) [0..3]

slots' :: [Slots]
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

--------------------------------------------------------------------------------

cspSolvable :: CSP Int Int
cspSolvable = toCSP slots events usersAvail

cspUnsolvable :: CSP Int Int
cspUnsolvable = toCSP slots' events usersAvail

countExpectedLength :: Int -> [M.Map Int a] -> Int
countExpectedLength n = length . filter ((==n) . M.size)

main :: IO ()
main = do
    -- count how many times /1000 the result is the best length
    resultsSolveable <- replicateM 1000 $ ifs cspSolvable M.empty
    putStrLn $ "Solvable Best: " ++ show (countExpectedLength 8 resultsSolveable)
    resultsUnsolveable <- replicateM 1000 $ ifs cspUnsolvable M.empty
    putStrLn $ "Unsolvable Best: " ++ show (countExpectedLength 6 resultsUnsolveable)

    -- benchmark solvable and unsolvable CSPs
    defaultMain
        [
            bgroup "Basic IFS Tests"
            [
                bench "solvable" $
                    nfIO (ifs cspSolvable M.empty),
                bench "unsolvable" $
                    nfIO (ifs cspUnsolvable M.empty)
            ]
        ]

--------------------------------------------------------------------------------
