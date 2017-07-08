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
import CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps

import Test.Tasty
import Test.Tasty.HUnit


main = defaultMain tests

type Expr = Expression String


tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Explicit transformations"
     [ testCase "𝑎 + 𝑏 * 𝑐  &~:  ㄖ+ㄈ :=: ㄈ+ㄖ" $
      (𝑎 + 𝑏 * 𝑐 &~: ㄖ+ㄈ :=: ㄈ+ㄖ) @?= (𝑏 * 𝑐 + 𝑎 :: Expr)
     , testCase "(𝑎+𝑏) * 𝑐  &~:  ㄖ+ㄈ :=: ㄈ+ㄖ" $
      ((𝑎+𝑏) * 𝑐 &~: ㄖ+ㄈ :=: ㄈ+ㄖ) @?= ((𝑏+𝑎) * 𝑐 :: Expr)
     , testCase "𝑎*𝑏 - 𝑐*𝑑  &~:  ㄖ*ㄈ :=: ㄈ*ㄖ" $
      (𝑎*𝑏 - 𝑐*𝑑 &~: ㄖ*ㄈ :=: ㄈ*ㄖ) @?= (𝑏*𝑎 - 𝑑*𝑐 :: Expr)
     , testCase "𝑎*𝑏 - 𝑐*𝑑  &~?  ㄖ*ㄈ :=: ㄈ*ㄖ" $
      (𝑎*𝑏 - 𝑐*𝑑 &~? ㄖ*ㄈ :=: ㄈ*ㄖ) @?= ([𝑏*𝑎 - 𝑐*𝑑, 𝑎*𝑏 - 𝑑*𝑐] :: [Expr])
     ]
  ]



