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

data Expr a
  = Zero
  | Succ a
  | Var String
  | Add a a
  | Mul a a
  | Pow a a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

succToNum :: Fix Expr -> Int
succToNum (Fix Zero) = 0
succToNum (Fix (Succ x)) = 1 + succToNum x
succToNum _ = error "cannot convert variable to number"

prettyPrint :: Fix Expr -> String
prettyPrint (Fix Zero) = "0"
prettyPrint (Fix (Succ x)) = show (1 + succToNum x)
prettyPrint (Fix (Var v)) = v
prettyPrint (Fix (Add x y)) = "(" ++ prettyPrint x ++ "+" ++ prettyPrint y ++ ")"
prettyPrint (Fix (Mul x y)) = "(" ++ prettyPrint x ++ "*" ++ prettyPrint y ++ ")"
prettyPrint (Fix (Pow x y)) = "(" ++ prettyPrint x ++ "^" ++ prettyPrint y ++ ")"

cost :: CostFunction Expr Int
cost Zero = 1
cost (Succ x) = 1 + x
cost (Var _) = 1
cost (Add x y) = 10 + x + y
cost (Mul x y) = 100 + x + y
cost (Pow x y) = 1000 + x + y

rewrites :: [Rewrite () Expr]
rewrites =
  [ pat (Add "x" "y") := pat (Add "y" "x"), -- add-commutative
    pat (Mul "x" "y") := pat (Mul "y" "x"), -- mul-commutative
    pat (Add (pat (Add "x" "y")) "z") := pat (Add "x" (pat (Add "y" "z"))), -- add-associative
    pat (Add "x" (pat (Add "y" "z"))) := pat (Add (pat (Add "x" "y")) "z"), -- add-associative2
    pat (Mul (pat (Mul "x" "y")) "z") := pat (Mul "x" (pat (Mul "y" "z"))), -- mul-associative
    pat (Mul "x" (pat (Mul "y" "z"))) := pat (Mul (pat (Mul "x" "y")) "z"), -- mul-associative2
    pat (Mul "x" (pat (Add "a" "b"))) := pat (Add (pat (Mul "x" "a")) (pat (Mul "x" "b"))), -- distributive
    pat (Add (pat (Mul "x" "a")) (pat (Mul "x" "b"))) := pat (Mul "x" (pat (Add "a" "b"))), -- distributive2
    pat (Mul "x" (pat (Succ (pat Zero)))) := "x", -- x*1 = x
    pat (Pow "x" (pat Zero)) := pat (Succ (pat Zero)), -- x^0 = 1
    pat (Pow "x" (pat (Succ "n"))) := pat (Mul "x" (pat (Pow "x" "n"))) -- x^(n+1) = x * x^n
  ]

rewrite :: Fix Expr -> Fix Expr
rewrite expr = fst (equalitySaturation' (BackoffScheduler 0 0) expr rewrites cost)

c0, c1, c2, c3, c4, c5 :: Expr (Fix Expr)
c0 = Zero

c1 = Succ (Fix c0)

c2 = Succ (Fix c1)

c3 = Succ (Fix c2)

c4 = Succ (Fix c3)

c5 = Succ (Fix c4)

a, b, c, d, e, f, x :: Expr (Fix Expr)
a = Var "a"

b = Var "b"

c = Var "c"

d = Var "d"

e = Var "e"

f = Var "f"

x = Var "x"

poly1, poly2, poly3, poly4, poly5 :: Fix Expr
poly1 = Fix (Add (Fix (Mul (Fix b) (Fix x))) (Fix a)) -- bx + a

poly2 = Fix (Add (Fix (Mul (Fix c) (Fix (Pow (Fix x) (Fix c2))))) poly1) -- cx^2 + bx + a

poly3 = Fix (Add (Fix (Mul (Fix d) (Fix (Pow (Fix x) (Fix c3))))) poly2) -- dx^3 + cx^2 + bx + a

poly4 = Fix (Add (Fix (Mul (Fix e) (Fix (Pow (Fix x) (Fix c4))))) poly3) -- ex^4 + dx^3 + cx^2 + bx + a

poly5 = Fix (Add (Fix (Mul (Fix f) (Fix (Pow (Fix x) (Fix c5))))) poly4) -- fx^5 + ex^4 + dx^3 + cx^2 + bx + a

main :: IO ()
main = do
  print poly5
  print (prettyPrint poly5)
  let r = rewrite poly5
  print r
  print (prettyPrint r)
  defaultMain
    [ bench "rewrite poly5" $ whnf rewrite poly5 ]