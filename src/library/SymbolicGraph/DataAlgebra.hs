module SymbolicGraph.DataAlgebra
  ( module SymbolicGraph.DataAlgebra.Core
  ) where

{-|
Description : Datatypes for representing the base algebra.
Copyright   : (c) Guilherme Azzi, 2017
License     : MIT
Stability   : experimental

The base algebra used for symbolic graphs is the quotient term
algebra for the following specification.

  SORTS
    Int

  OPERATIONS
    0 : Int
    succ : Int -> Int
    pred : Int -> Int
    neg : Int -> Int
    add : Int -> Int -> Int
    sub : Int -> Int -> Int
    mul : Int -> Int -> Int

  RestrictionS
    FORALL m n : Int

    . succ(pred(n)) = n
    . pred(succ(n)) = n

    . neg(0) = 0
    . neg(succ(n)) = pred(neg(n))
    . neg(pred(n)) = succ(neg(n))

    . add(n, 0) = n
    . add(n, succ(m)) = succ(add(n, m))
    . add(n, pred(m)) = pred(add(n, m))

    . sub(n, m) = add(n, neg(m))

    . mul(n, 0) = 0
    . mul(n, succ(m)) = add(n, mul(n, m))
-}

import           SymbolicGraph.DataAlgebra.Core
import           SymbolicGraph.DataAlgebra.PrettyPrint ()
