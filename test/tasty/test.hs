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
{-# LANGUAGE CPP              #-}

module Main where

import CAS.Dumb

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
     , testCase "Rename local symbols" $
      (map succ%$> 𝑎+𝑝) * 𝑥  @?=  ((𝑏+𝑞) * 𝑥 :: Expr)
     ]
  , testGroup "Show instance"
     [ testCase "𝑎+𝑏*𝑐" $
      show (𝑎+𝑏*𝑐 :: Expr) @?= "𝑎+𝑏*𝑐"
     , testCase "(𝑎+𝑏)*𝑐" $
      show ((𝑎+𝑏)*𝑐 :: Expr) @?= "(𝑎+𝑏)*𝑐"
     , testCase "abs (𝑎+𝑏)" $
      show (abs (𝑎+𝑏) :: Expr) @?= "abs (𝑎+𝑏)"
     , testCase "abs 3" $
      show (abs 3 :: Expr) @?= "abs 3"
     , testCase "𝑎 + -3" $
      show (𝑎+(-3) :: Expr) @?= "𝑎+( -3)"
     , testCase "𝑎 / signum π" $
      show (𝑎/signum π :: Expr) @?= "𝑎/signum π"
     ]
  ]



