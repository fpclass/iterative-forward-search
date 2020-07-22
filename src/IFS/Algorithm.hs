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

import qualified Data.Set as S

import IFS.Types

--------------------------------------------------------------------------------

canContinue :: CSP -> Assignment -> Bool
canContinue = undefined

selectVariable :: CSP -> Assignment -> IO Int
selectVariable = undefined

selectValue :: CSP -> Assignment -> Int -> IO Int
selectValue = undefined

conflicts :: CSP -> Assignment -> Int -> Int -> IO (S.Set Int)
conflicts = undefined

isBetterThan :: CSP -> Assignment -> Assignment -> Bool
isBetterThan = undefined

-- | `ifs'` @csp currAssign bestAssign@ checks whether it should continue the
-- search given @currAssign@, and if so performs the next iteration of the IFS
-- algorithm and recursively calls this function again with the new assignment.
-- If `canContinue` returns false the best assignment found so far is returned
ifs' :: CSP -> Assignment -> Assignment -> IO Assignment
ifs' csp currAssign bestAssign
    | canContinue csp currAssign = do
        var <- selectVariable csp currAssign
        val <- selectValue csp currAssign var
        conflictingVars <- conflicts csp currAssign var val
        let newAssignment = undefined
        
        if isBetterThan csp newAssignment bestAssign      
        then ifs' csp newAssignment newAssignment
        else ifs' csp newAssignment bestAssign
    | otherwise                  = pure bestAssign

-- | `ifs` @csp startingAssignment@ performs an iterative first search on @csp@
-- using @startingAssignment@ as the initial assignment
ifs :: CSP -> Assignment -> IO Assignment
ifs csp startingAssignment = ifs' csp startingAssignment startingAssignment
