--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module IFS.Algorithm (
    ifs
) where

--------------------------------------------------------------------------------

import qualified Data.IntMap as M
import qualified Data.Set as S

import System.Random

import IFS.Types

--------------------------------------------------------------------------------

-- | `canContinue` @csp iterations currAssign@ determines whether to continue
-- the algorithm or terminate. It terminates if the current assignment assigns
-- all variables or the maximum number of iterations has been exceded
canContinue :: CSP -> Int -> Assignment -> Bool
canContinue CSP{..} iterations currAssign =
    M.size cspDomains == M.size currAssign || iterations >= cspMaxIterations

selectVariable :: CSP -> Assignment -> IO Int
selectVariable = undefined

selectValue :: CSP -> Assignment -> Int -> IO Int
selectValue = undefined

conflicts :: CSP -> Assignment -> Int -> Int -> IO Variables
conflicts = undefined

-- | `getBest` @newAssign bestAssign@ determines whether @newAssign@ is better
-- than @bestAssign@ and returns the best out of the two. A random is picked
-- if both are deemed equally good
getBest :: Assignment -> Assignment -> IO Assignment
getBest newAssign bestAssign =
    case M.size newAssign `compare` M.size bestAssign of
        -- if more variables are assigned in the current assignment it is better
        GT -> pure newAssign
        -- if less variables are assigned it is worse
        LT -> pure bestAssign
        -- if both have an equal number of variables assigned pick randomly
        EQ -> do
            useNew <- (<= 0.5) <$> (randomIO :: IO Double)
            pure $ if useNew then newAssign else bestAssign

-- | `ifs'` @csp iterations currAssign bestAssign@ checks whether it should
-- continue the search given @currAssign@, and if so performs the next iteration
-- of the IFS algorithm and recursively calls this function again with the new
-- assignment. If `canContinue` returns false the best assignment found so far
-- is returned
ifs' :: CSP -> Int -> Assignment -> Assignment -> IO Assignment
ifs' csp iterations currAssign bestAssign
    | canContinue csp iterations currAssign = do
        -- get variable to change
        var <- selectVariable csp currAssign

        -- get new value for @var@
        val <- selectValue csp currAssign var

        -- find conflicting variables
        conflictingVars <- conflicts csp currAssign var val

        -- unassign conflicting variables
        let newAssignment = foldr M.delete currAssign conflictingVars
        
        -- run ifs' with the new assignment
        ifs' csp (iterations+1) newAssignment =<< getBest newAssignment bestAssign

    | otherwise = pure bestAssign

-- | `ifs` @csp startingAssignment@ performs an iterative first search on @csp@
-- using @startingAssignment@ as the initial assignment
ifs :: CSP -> Assignment -> IO Assignment
ifs csp startingAssignment = ifs' csp 0 startingAssignment startingAssignment
