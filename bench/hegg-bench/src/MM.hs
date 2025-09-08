{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler
import Data.Equality.Utils

data Linalg a
  = Mat Int Int
  | Mul a a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

-- struct DimAndCost that has 3 fields: rows, cols, cost, that I can refer to by name
data DimAndCost = DimAndCost
  { rows :: Int,
    cols :: Int,
    costVal :: Int
  }
  deriving (Show, Eq, Ord)

cost :: CostFunction Linalg DimAndCost
cost (Mat r c) = DimAndCost r c 0
cost (Mul x y) = let DimAndCost r1 c1 cost1 = x
                     DimAndCost r2 c2 cost2 = y
                   in if c1 /= r2
                        then error "matrix dimensions do not match"
                        else DimAndCost r1 c2 (cost1 + cost2 + r1 * c1 * c2)

rewrites :: [Rewrite () Linalg]
rewrites =
  [ pat (Mul (pat (Mul "x" "y")) "z") := pat (Mul "x" (pat (Mul "y" "z"))), -- mul-associative
    pat (Mul "x" (pat (Mul "y" "z"))) := pat (Mul (pat (Mul "x" "y")) "z") -- mul-associative2
  ]

rewrite :: Fix Linalg -> Fix Linalg
rewrite expr = fst (equalitySaturation' (BackoffScheduler 0 0) expr rewrites cost)

mm :: Int -> Fix Linalg -- if n == 0, then return Mat(10, 10), else Mul(mm(n-1), Mat(10, 10))
mm 0 = Fix (Mat 10 10)
mm n = Fix (Mul (mm (n -1)) (Fix (Mat 10 10)))

main :: IO ()
main = do
  defaultMain -- 3, 5, 10, 20, 40, 80
    [ bench "rewrite mm3" $ whnf rewrite (mm 3),
      bench "rewrite mm5" $ whnf rewrite (mm 5),
      bench "rewrite mm10" $ whnf rewrite (mm 10),
      bench "rewrite mm20" $ whnf rewrite (mm 20),
      bench "rewrite mm40" $ whnf rewrite (mm 40),
      bench "rewrite mm80" $ whnf rewrite (mm 80)
    ]