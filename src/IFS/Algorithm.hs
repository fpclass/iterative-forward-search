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

import           Control.Arrow              ( Arrow ((&&&)) )
import           Control.Monad.Trans.Class  ( MonadTrans (lift) )
import           Control.Monad.Trans.Reader

import qualified Data.IntMap                as M
import           Data.Maybe                 ( fromJust )
import qualified Data.Set                   as S

import           System.Random

import           Control.Monad              ( when )
import           IFS.Types

--------------------------------------------------------------------------------

-- | `canContinue` @iterations currAssign@ determines whether to continue the
-- algorithm or terminate. It terminates if the current assignment assigns all
-- variables or the maximum number of iterations has been exceded
canContinue :: Int -> Assignment -> CSPMonad Bool
canContinue iterations currAssign =
    -- get domains and max interations
    (cspDomains &&& cspMaxIterations) <$> ask >>= \(doms, max) ->
    -- check conditions
    pure $ M.size doms > M.size currAssign && iterations <= max

-- | `selectVariable` @currAssignment@ decides which variable to change next
selectVariable :: Assignment -> CSPMonad Int
selectVariable currAssignment = do
    -- get CSP parameters
    CSP{..} <- ask

    -- get variables currently not assigned. We can assume this is non-empty
    -- as the algorithm terminates when all are assigned
    let unassigned = cspVariables S.\\ S.fromList (M.keys currAssignment)

    -- index these variables by size of domain - # connected constraints. The
    -- lowest index is then the most restricted variable
    -- TODO: This scaling one of these numbers could be better
    let restricted = M.fromListWith (++) $ flip map (S.toList unassigned) $ \var ->
            (S.size (cspDomains M.! var) - countConnectedCons var cspConstraints, [var])

    -- TODO: This doesn't work for CSPs without a feasible solution, for example
    -- if 2 constraints conflict it won't find the maximal working solution, it will
    -- never select variables that are deemed less important than the conflict
    -- get most restricted variables
    let toChoseFrom = snd $ M.findMin restricted

    -- pick a random variable from these
    (toChoseFrom !!) <$> lift (randomRIO (0, length toChoseFrom - 1))

    where
        -- counts the number of constraints connected to @var@
        countConnectedCons var = flip foldl 0 $ \conflicting (conVars, _) ->
            if var `S.member` conVars
            then conflicting + 1
            else conflicting

-- | `setValue` @csp currAssign var@ determines a value to assign to @var@ and
-- returns @currAssign@ with @var@ assigned to the determined value
setValue :: Assignment -> Int -> CSPMonad Assignment
setValue currAssign var = do
    (doms, cons) <- (cspDomains &&& cspConstraints) <$> ask
    let domain = flip S.filter (fromJust $ M.lookup var doms) $ \val ->
            countConflicts (M.singleton var val) cons == 0

    -- TODO: Probably something better than this
    when (null domain) $ error "CSP Cannot Be Solved"

    -- create map with key of the number of contraints violated, and the value
    -- being a list of assignments with that number of conflicts
    let conflictMap = M.fromListWith (++) $ flip map (S.toList domain) $ \val ->
            let assignment = M.insert var val currAssign
            in (countConflicts assignment cons, [assignment])

    -- TODO: Some kind of nice formula - weight the smaller conflicts more
    -- and the weighting should be heaver if the gap between nums of conflicts
    -- is larger
    -- Will chose from the 10% of assignments with the lowest number of conflicts
    let cap = ceiling $ 0.1 * fromIntegral (S.size domain) -- :: Int

    -- get at least @cap@ assignments in order of conflicts
    let toChoseFrom = getToChoseFrom 0 cap [] conflictMap

    -- get a radndom assignment from this list
    (toChoseFrom !!) <$> lift (randomRIO (0, length toChoseFrom - 1))

    where
        -- counts the number of conflicts in @assignment@
        countConflicts assignment = flip foldl 0 $
            \conflicting (_, constraintF) ->
                if constraintF assignment
                then conflicting
                else conflicting + 1

        -- gets lowest number of assignments >cap possible when sorting by
        -- conflict number
        getToChoseFrom :: Int -> Int -> [Assignment] -> M.IntMap [Assignment] -> [Assignment]
        getToChoseFrom n cap added toAdd
            | n >= cap   = added
            | otherwise = let ((_,as), toAdd') = M.deleteFindMin toAdd
                          in getToChoseFrom (n + length as) cap
                                (added ++ as) toAdd'

-- | `removeConflicts` @currAssign var@ checks which constraints from @csp@
-- are violated by @currAssign@ and removes all variables involved in the
-- violated constraints except @var@
removeConflicts :: Assignment -> Int -> CSPMonad Assignment
removeConflicts currAssignment var = cspConstraints <$> ask >>= \cons ->
    -- check each constraint and if it is violated add the variables involved
    -- other than @var@ to the set of conflicting variables
    -- TODO: Something more intelligent
    let conflictingVars = flip (flip foldl S.empty) cons $
            \conflicting (constraintVars, constraintF) ->
                if constraintF currAssignment
                then conflicting
                else conflicting `S.union` S.delete var constraintVars

    -- unassign conflicting variables
    in pure $ foldr M.delete currAssignment conflictingVars

-- | `getBest` @newAssign bestAssign@ determines whether @newAssign@ is better
-- than @bestAssign@ and returns the best out of the two. A random is picked
-- if both are deemed equally good
getBest :: Assignment -> Assignment -> CSPMonad Assignment
getBest newAssign bestAssign =
    case M.size newAssign `compare` M.size bestAssign of
        -- if more variables are assigned in the current assignment it is better
        GT -> pure newAssign
        -- if less variables are assigned it is worse
        LT -> pure bestAssign
        -- if both have an equal number of variables assigned pick randomly
        EQ -> do
            useNew <- (<= 0.5) <$> (lift randomIO :: CSPMonad Double)
            pure $ if useNew then newAssign else bestAssign

-- | `ifs'` @iterations currAssign bestAssign@ checks whether it should continue
-- the search given @currAssign@, and if so performs the next iteration of the
-- IFS algorithm and recursively calls this function again with the new
-- assignment. If `canContinue` returns false the best assignment found so far
-- is returned
ifs' :: Int -> Assignment -> Assignment -> CSPMonad Assignment
ifs' iterations currAssign bestAssign = canContinue iterations currAssign >>= \continue ->
    if continue
    then do
        -- get variable to change
        var <- selectVariable currAssign

        -- determine and set new value for @var@
        newAssignment <- setValue currAssign var

        -- find and unassign conflicting variables
        conflictsRemoved <- removeConflicts newAssignment var

        -- run ifs' with the new assignment
        ifs' (iterations+1) conflictsRemoved =<< getBest conflictsRemoved bestAssign

    else pure bestAssign

-- | `ifs` @csp startingAssignment@ performs an iterative first search on @csp@
-- using @startingAssignment@ as the initial assignment
ifs :: CSP -> Assignment -> IO Assignment
ifs csp startingAssignment = runReaderT (ifs' 0 startingAssignment startingAssignment) csp
