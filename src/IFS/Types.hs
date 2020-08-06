--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module IFS.Types (
    CSPMonad,
    CSP(..),
    Domains,
    Variables,
    Constraints,
    Assignment
) where

--------------------------------------------------------------------------------

import           Control.Monad.Trans.Reader ( ReaderT )

import qualified Data.Map                   as M
import qualified Data.Set                   as S

--------------------------------------------------------------------------------

-- | Monad used in CSP solver
type CSPMonad var val = ReaderT (CSP var val) IO

-- | Represents a contraint satisfaction problem
data CSP var val = CSP{
    cspDomains     :: Domains var val,
    cspVariables   :: Variables var,
    cspConstraints :: Constraints var val,
    cspRandomCap   :: Int,
    cspTermination :: Maybe (Int -> Assignment var val -> CSPMonad var val Bool)
}

-- | Represents the domains for different variables. The variables are indexed
-- by integers
type Domains var val = M.Map var (S.Set val)

-- | Represents the variables used in the timetabling problem
type Variables var = S.Set var

-- | Represents the constraints for the timetabling problem. The first element
-- of the tuple represents the variables this constraint affects, the second is
-- the constraint itself
type Constraints var val = [(Variables var, Assignment var val -> Bool)]

-- | Represents an assignment of variables
type Assignment var val = M.Map var val
