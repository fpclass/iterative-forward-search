--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.IFS.Algorithm (
    ifs
) where

--------------------------------------------------------------------------------

import           Control.Arrow              ( Arrow ((&&&)) )
import           Control.Monad              ( when )
import           Control.Monad.Trans.Class  ( MonadTrans (lift) )
import           Control.Monad.Trans.Reader

import           Data.Foldable              ( foldlM )
import qualified Data.IntMap                as M
import qualified Data.IntSet                as S
import           Data.Maybe                 ( fromJust, fromMaybe )

import           System.Random

import           Data.IFS.Types

--------------------------------------------------------------------------------

-- | `defaultCanContinue` @iterations currAssign@ determines whether to continue
-- the algorithm or terminate. It terminates if the current assignment assigns
-- all variables or the maximum number of iterations has been exceded
defaultCanContinue :: Int -> Assignment -> CSPMonad Assignment (Maybe Assignment)
defaultCanContinue iterations currAssign =
    -- get variables
    cspVariables <$> ask >>= \vars ->
    -- check conditions
    if S.size vars > M.size currAssign && iterations <= 25 * S.size vars
    then pure Nothing
    else pure $ Just currAssign

-- | `getMostRestricted` @vars doms cons@ indexes these variables by size of
-- domain - # connected constraints. The lowest index is then the most
-- restricted variable.
getMostRestricted :: Variables
                  -> Domains
                  -> Constraints
                  -> M.IntMap [Var]
getMostRestricted vars doms cons =
    -- TODO: This scaling one of these numbers could be better
    M.fromListWith (++) $ flip map (S.toList vars) $ \var ->
            (S.size (doms M.! var) - countConnectedCons var cons, [var])

    where
        -- counts the number of constraints connected to @var@
        countConnectedCons var = flip foldl 0 $ \conflicting (conVars, _) ->
            if var `S.member` conVars
            then conflicting + 1
            else conflicting

-- | `selectVariable` @currAssignment@ decides which variable to change next
selectVariable :: Int -> Assignment -> CSPMonad r Var
selectVariable iterations currAssignment = do
    -- get CSP parameters
    CSP{..} <- ask

    -- get variables currently not assigned. We can assume this is non-empty
    -- as the algorithm terminates when all are assigned
    let unassigned = cspVariables S.\\ S.fromList (M.keys currAssignment)

    -- find which of these is most restricted
    let restricted = getMostRestricted unassigned cspDomains cspConstraints

    -- if we are before the random cap then pick one of the most difficult
    -- variables
    if iterations < cspRandomCap
    then
        -- pick a random variable from the most difficult
        let toChoseFrom = snd $ M.findMin restricted
        in (toChoseFrom !!) <$> lift (randomRIO (0, length toChoseFrom - 1))
    else
        -- pick any random variable
        let unassignedList = S.toList unassigned
        in (unassignedList !!) <$> lift (randomRIO (0, length unassignedList - 1))

-- | `setValue` @csp currAssign var@ determines a value to assign to @var@ and
-- returns @currAssign@ with @var@ assigned to the determined value
setValue :: Assignment
         -> Var
         -> CSPMonad r Assignment
setValue currAssign var = do
    (doms, cons) <- (cspDomains &&& cspConstraints) <$> ask
    let domain = flip S.filter (fromJust $ M.lookup var doms) $ \val ->
            countConflicts (M.singleton var val) cons == 0

    -- If no possible values return current assignment unchanged
    if S.null domain
    then pure currAssign
    else do
        -- create map with key of the number of contraints violated, and the
        -- value being a list of assignments with that number of conflicts
        let conflictMap = M.fromListWith (++) $ flip map (S.toList domain) $ \val ->
                -- whether all must be distinct should be an option
                let assignment = M.insert var val currAssign -- $ M.filter (/= val) currAssign
                in (countConflicts assignment cons, [assignment])

        -- TODO: Some kind of nice formula - weight the smaller conflicts more
        -- and the weighting should be heaver if the gap between nums of conflicts
        -- is larger
        -- Will chose from the 10% of assignments with the lowest number of
        -- conflicts
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
        getToChoseFrom :: Int
                       -> Int
                       -> [Assignment]
                       -> M.IntMap [Assignment]
                       -> [Assignment]
        getToChoseFrom n cap added toAdd
            | n >= cap   = added
            | otherwise = let ((_,as), toAdd') = M.deleteFindMin toAdd
                          in getToChoseFrom (n + length as) cap
                                (added ++ as) toAdd'

-- | `removeConflicts'` @var assign constraintF toRemove@ repeated unassigns
-- one of the least constrained variables except @var@ from @assign@ until the
-- @constraintF@ passes
removeConflicts' :: Var
                 -> Assignment
                 -> (Assignment -> Bool)
                 -> M.IntMap [Var]
                 -> Assignment
removeConflicts' var assign constraintF toRemove
    | constraintF assign = assign
    | otherwise          =
        let
            -- get next minimum variables
            (_, x:remaining) = M.findMax toRemove
            -- remove this variable from the map
            toRemove' = if null remaining
                        then snd $ M.deleteFindMax toRemove
                        else M.updateMax (const $ Just remaining) toRemove
            -- unassign this variable unless it is the variable just assigned
            newAssign = if x==var then assign else M.delete x assign
        in removeConflicts' var newAssign constraintF toRemove'

-- | `removeConflicts` @currAssign var@ checks which constraints from @csp@
-- are violated by @currAssign@ and removes all variables involved in the
-- violated constraints except @var@
removeConflicts :: Assignment
                -> Var
                -> CSPMonad r Assignment
removeConflicts currAssignment var =
    (cspDomains &&& cspConstraints) <$> ask >>= \(doms, cons) ->
    -- check each constraint and if it is violated unassign variables until
    -- the constraint passes
    pure $ flip (flip foldl currAssignment) cons $
            \assign (constraintVars, constraintF) ->
                if constraintF assign
                then assign
                else removeConflicts' var assign constraintF
                        $ getMostRestricted constraintVars doms cons

-- | `getBest` @newAssign bestAssign@ determines whether @newAssign@ is better
-- than @bestAssign@ and returns the best out of the two. A random is picked
-- if both are deemed equally good
getBest :: Assignment
        -> Assignment
        -> CSPMonad r Assignment
getBest newAssign bestAssign =
    case M.size newAssign `compare` M.size bestAssign of
        -- if more variables are assigned in the current assignment it is better
        GT -> pure newAssign
        -- if less variables are assigned it is worse
        LT -> pure bestAssign
        -- if both have an equal number of variables assigned pick randomly
        EQ -> do
            useNew <- (<= 0.5) <$> (lift randomIO :: CSPMonad r Double)
            pure $ if useNew then newAssign else bestAssign

-- | `ifs'` @iterations currAssign bestAssign@ checks whether it should continue
-- the search given @currAssign@, and if so performs the next iteration of the
-- IFS algorithm and recursively calls this function again with the new
-- assignment. If `canContinue` returns false the best assignment found so far
-- is returned
ifs' :: Int
     -> Assignment
     -> Assignment
     -> CSPMonad r r
ifs' iterations currAssign bestAssign = do
    canContinue <- cspTermination <$> ask
    continue <- canContinue iterations bestAssign
    case continue of
        Nothing -> do
            -- get variable to change
            var <- selectVariable iterations currAssign

            -- determine and set new value for @var@
            newAssignment <- setValue currAssign var

            -- find and unassign conflicting variables
            conflictsRemoved <- removeConflicts newAssignment var

            -- run ifs' with the new assignment
            ifs' (iterations+1) conflictsRemoved =<< getBest conflictsRemoved bestAssign

        Just a -> pure a

-- | `ifs` @csp startingAssignment@ performs an iterative first search on @csp@
-- using @startingAssignment@ as the initial assignment
ifs :: CSP r
    -> Assignment
    -> IO r
ifs csp startingAssignment =
    runReaderT (ifs' 0 startingAssignment startingAssignment) csp

--------------------------------------------------------------------------------
