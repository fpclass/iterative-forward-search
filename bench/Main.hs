--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

import           Criterion.Main

import           Control.Monad

import qualified Data.IntMap    as M

import           IFS.Algorithm
import           IFS.Timetable
import           IFS.Types

--------------------------------------------------------------------------------

cspSolvable :: CSP
cspSolvable = toCSP slots groups usersAvail

cspUnsolvable :: CSP
cspUnsolvable = toCSP slots' groups usersAvail

countExpectedLength :: Int -> [M.IntMap a] -> Int
countExpectedLength n = length . filter ((==n) . M.size)

main :: IO ()
main = do
    -- count how many times /1000 the result is the best length
    resultsSolveable <- replicateM 1000 $ ifs cspSolvable M.empty
    putStrLn $ "Solvable Best: " ++ show (countExpectedLength 8 resultsSolveable)
    resultsUnsolveable <- replicateM 1000 $ ifs cspUnsolvable M.empty
    putStrLn $ "Unsolvable Best: " ++ show (countExpectedLength 4 resultsUnsolveable)

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
