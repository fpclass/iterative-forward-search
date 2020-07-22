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
import Data.Maybe (fromJust)
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

-- | `setValue` @csp currAssign var@ determines a value to assign to @var@ and
-- returns @currAssign@ with @var@ assigned to the determined value
setValue :: CSP -> Assignment -> Int -> IO Assignment
setValue CSP{..} currAssign var = do
    let domain = fromJust $ M.lookup var cspDomains

    -- create map with key of the number of contraints violated, and the value
    -- being a list of assignments with that number of conflicts
    let conflictMap = M.fromListWith (++) $ flip map (S.toList domain) $ \val ->
            let assignment = M.insert var val currAssign
            in (countConflicts assignment, [assignment])
    
    -- TODO: Some kind of nice formula - weight the smaller conflicts more
    -- and the weighting should be heaver if the gap between nums of conflicts
    -- is larger
    -- Will chose from the 10% of assignments with the lowest number of conflicts
    let cap = ceiling $ 0.1 * fromIntegral (S.size domain) -- :: Int
    
    -- get at least @cap@ assignments in order of conflicts
    let toChoseFrom = getToChoseFrom 0 cap [] conflictMap
    
    -- get a radndom assignment from this list
    (toChoseFrom !!) <$> randomRIO (0, length toChoseFrom - 1)

    where
        -- counts the number of conflicts in @assignment@
        countConflicts assignment = flip (flip foldl 0) cspConstraints $
            \conflicting (_, constraintF) ->
                if constraintF assignment
                then conflicting
                else conflicting + 1
        
        -- gets lowest number of assignments >cap possible when sorting by
        -- conflict number
        getToChoseFrom :: Int -> Int -> [Assignment] -> M.IntMap [Assignment] -> [Assignment]
        getToChoseFrom n cap added toAdd
            | n > cap   = added
            | otherwise = let (as, toAdd') = M.deleteFindMin toAdd
                          in getToChoseFrom (n + length as) cap (added ++ snd as) toAdd'

-- | `removeConflicts` @csp currAssign var@ checks which constraints from @csp@
-- are violated by @currAssign@ and removes all variables involved in the 
-- violated constraints except @var@
removeConflicts :: CSP -> Assignment -> Int -> Assignment
removeConflicts CSP{..} currAssignment var =
    -- check each constraint and if it is violated add the variables involved
    -- other than @var@ to the set of conflicting variables
    -- TODO: Something more intelligent
    let conflictingVars = flip (flip foldl S.empty) cspConstraints $
            \conflicting (constraintVars, constraintF) ->
                if constraintF currAssignment
                then conflicting
                else conflicting `S.union` S.delete var constraintVars

    -- unassign conflicting variables
    in foldr M.delete currAssignment conflictingVars

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

        -- determine and set new value for @var@
        newAssignment <- setValue csp currAssign var

        -- find and unassign conflicting variables
        let conflictsRemoved = removeConflicts csp newAssignment var
        
        -- run ifs' with the new assignment
        ifs' csp (iterations+1) conflictsRemoved =<< getBest conflictsRemoved bestAssign

    | otherwise = pure bestAssign

-- | `ifs` @csp startingAssignment@ performs an iterative first search on @csp@
-- using @startingAssignment@ as the initial assignment
ifs :: CSP -> Assignment -> IO Assignment
ifs csp startingAssignment = ifs' csp 0 startingAssignment startingAssignment
