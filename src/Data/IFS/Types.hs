--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE file in  --
-- the root directory of this source tree.                                    --
--------------------------------------------------------------------------------

module Data.IFS.Types (
    Var,
    Val,
    CSPMonad,
    CSP(..),
    Domains,
    Variables,
    Constraints,
    Assignment,
    Solution(..),
    fromSolution
) where

--------------------------------------------------------------------------------

import           Control.DeepSeq
import           Control.Monad.Trans.Reader ( ReaderT )

import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS

--------------------------------------------------------------------------------

-- | Represents a variable
type Var = Int

-- | Represents a value
type Val = Int

-- | Monad used in the CSP solver
type CSPMonad r = ReaderT (CSP r) IO

-- | Represents a contraint satisfaction problem
data CSP r = MkCSP{
    cspDomains     :: Domains,
    cspVariables   :: Variables,
    cspConstraints :: Constraints,
    -- | The number of iterations the algorithm should perform before selecting
    -- unassigned variables at random instead of picking one of the most
    -- constrained (to avoid getting stuck in a loop)
    cspRandomCap   :: Int,
    -- | When to terminate and return the current assignment, given the number
    -- of iterations performed and the current assignment. A `Nothing` value
    -- means continue, and a `Just` value means terminate and return that
    -- value
    cspTermination :: Int -> Assignment -> CSPMonad r (Maybe r)
}

-- | Represents the domains for different variables. The variables are indexed
-- by integers
type Domains = IM.IntMap IS.IntSet

-- | Represents the variables used in the timetabling problem
type Variables = IS.IntSet

-- | Represents the constraints for the timetabling problem. The first element
-- of the tuple represents the variables this constraint affects, the second is
-- the constraint itself
type Constraints = [(Variables, Assignment -> Bool)]

-- | Represents an assignment of variables
type Assignment = IM.IntMap Val

-- | This is returned by the IFS. `Complete` indicates the assignment is
-- complete, and `Incomplete` indicates the assignment is not complete
data Solution
    = Complete Assignment
    | Incomplete Assignment
    deriving Show

instance NFData Solution where
    rnf = rnf . fromSolution

-- | `fromSolution` @solution@ extracts an `Assignment` value from a `Solution`
-- value
fromSolution :: Solution -> Assignment
fromSolution (Complete a)   = a
fromSolution (Incomplete a) = a

--------------------------------------------------------------------------------
