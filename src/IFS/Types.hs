--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module IFS.Types (
    Var,
    Val,
    CSPMonad,
    CSP(..),
    Domains,
    Variables,
    Constraints,
    Assignment
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
type CSPMonad = ReaderT CSP IO

-- | Represents a contraint satisfaction problem
data CSP = CSP{
    cspDomains     :: Domains,
    cspVariables   :: Variables,
    cspConstraints :: Constraints,
    cspRandomCap   :: Int,
    cspTermination :: Maybe (Int -> Assignment -> CSPMonad Bool)
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

--------------------------------------------------------------------------------
