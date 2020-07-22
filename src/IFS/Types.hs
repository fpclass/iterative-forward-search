--------------------------------------------------------------------------------
-- Iterative Forward Search                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the terms found in the LICENSE          --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module IFS.Types (
    CSP(..),
    Domains,
    Variables,
    Constraints,
    Assignment
) where

--------------------------------------------------------------------------------

import qualified Data.IntMap as M
import qualified Data.Set as S

--------------------------------------------------------------------------------

-- | Represents a contraint satisfaction problem
data CSP = CSP Domains Variables Constraints

-- | Represents the domains for different variables. The variables are indexed
-- by integers
type Domains = M.IntMap (S.Set Int)

-- | Represents the variables used in the timetabling problem
type Variables = S.Set Int

-- | Represents the constraints for the timetabling problem. Each constraint
-- applies to 1 variable
type Constraints = M.IntMap [Int -> Bool]

-- | Represents an assignment of variables
type Assignment = M.IntMap Int
