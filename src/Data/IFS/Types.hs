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

import           Control.Monad.Trans.Reader ( ReaderT )

import qualified Data.IntMap                as M
import qualified Data.IntSet                as S

--------------------------------------------------------------------------------

-- | Represents a variable
type Var = Int

-- | Represents a value
type Val = Int

-- | Monad used in CSP solver
type CSPMonad r = ReaderT (CSP r) IO

-- | Represents a contraint satisfaction problem
data CSP r = CSP{
    cspDomains     :: Domains,
    cspVariables   :: Variables,
    cspConstraints :: Constraints,
    cspRandomCap   :: Int,
    cspTermination :: Int -> Assignment -> CSPMonad r (Maybe r)
}

-- | Represents the domains for different variables. The variables are indexed
-- by integers
type Domains = M.IntMap (S.IntSet)

-- | Represents the variables used in the timetabling problem
type Variables = S.IntSet

-- | Represents the constraints for the timetabling problem. The first element
-- of the tuple represents the variables this constraint affects, the second is
-- the constraint itself
type Constraints = [(Variables, Assignment -> Bool)]

-- | Represents an assignment of variables
type Assignment = M.IntMap Val

-- | This is returned by the IFS. Solution indicates the assignment is complete,
-- and Incomplete indicates the assignment is not complete
data Solution
    = Solution Assignment
    | Incomplete Assignment

-- | `fromSolution` @solution@ extracts an Assignment value from a Solution
-- value
fromSolution :: Solution -> Assignment
fromSolution (Solution a)   = a
fromSolution (Incomplete a) = a

--------------------------------------------------------------------------------
