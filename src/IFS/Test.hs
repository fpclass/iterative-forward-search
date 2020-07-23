module IFS.Test where

import qualified Data.IntMap   as M
import           Data.Maybe    ( fromMaybe )
import qualified Data.Set      as S

import           IFS.Algorithm
import           IFS.Types

vars :: Variables
vars = S.fromList [1..5]

doms :: Domains
doms = foldl (\m i -> M.insert i (S.fromList [1..20]) m) M.empty [1..5]

-- BROKEN:
cons' :: Constraints
cons' =
    [
        (S.fromList [1], \a -> a M.!? 1 `elem` [Just 19, Nothing]),
        (S.fromList [2], \a -> a M.!? 2 `elem` Nothing:map Just [1..10]),
        (S.fromList [1,2], \a -> fromMaybe True $ (<) <$> a M.!? 1 <*> a M.!? 2),
        (S.fromList [3], \a -> a M.!? 2 `elem` Nothing:map Just [1..10]),
        (S.fromList [4], \a -> a M.!? 2 `elem` Nothing:map Just [1..10]),
        (S.fromList [5], \a -> a M.!? 2 `elem` Nothing:map Just [1..10])
    ]

cons :: Constraints
cons =
    [
        (S.fromList [1], \a -> a M.!? 1 `elem` [Just 15, Nothing]),
        (S.fromList [2], \a -> a M.!? 2 `elem` Nothing:map Just [11..20]),
        (S.fromList [1,2], \a -> fromMaybe True $ (>) <$> a M.!? 1 <*> a M.!? 2)
    ]

testCSP :: CSP
testCSP = CSP doms vars cons 20

doTest :: IO Assignment
doTest = ifs testCSP M.empty
