-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE LambdaCase       #-}

module Main where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols.ASCII

import Test.Tasty
import Test.Tasty.HUnit


main = defaultMain tests


type Expr = CAS InfixSymbol SEncapsulation Symbol


tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Explicit transformations"
     [ testCase "a + b * c &~: 𝑥+𝑦 :=: 𝑦+𝑥" $
      (a + b * c &~: _x+_y :=: _y+_x) @?= (b * c + a :: Expr)
     ]
  ]



