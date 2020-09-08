-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE CPP                     #-}

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
      (𝑎 + 𝑏 * 𝑐 &~: ㄖ+ㄈ :=: ㄈ+ㄖ) %@?= (𝑏 * 𝑐 + 𝑎 :: Expr)
     , testCase "(𝑎+𝑏) * 𝑐  &~:  ㄖ+ㄈ :=: ㄈ+ㄖ" $
      ((𝑎+𝑏) * 𝑐 &~: ㄖ+ㄈ :=: ㄈ+ㄖ) %@?= ((𝑏+𝑎) * 𝑐 :: Expr)
     , testCase "𝑎*𝑏 - 𝑐*𝑑  &~:  ㄖ*ㄈ :=: ㄈ*ㄖ" $
      (𝑎*𝑏 - 𝑐*𝑑 &~: ㄖ*ㄈ :=: ㄈ*ㄖ) %@?= (𝑏*𝑎 - 𝑑*𝑐 :: Expr)
     , testCase "𝑎*𝑏 - 𝑐*𝑑  &~?  ㄖ*ㄈ :=: ㄈ*ㄖ" $
      (𝑎*𝑏 - 𝑐*𝑑 &~? ㄖ*ㄈ :=: ㄈ*ㄖ) @?= [𝑏*𝑎 - 𝑐*𝑑, 𝑎*𝑏 - 𝑑*𝑐 :: Expr]
     , testCase "𝑎 + 𝑏 + 𝑐 + 𝑑  &~:  ㄜ+ㄑ :=: ㄑ+ㄜ" $
      (𝑎 + 𝑏 + 𝑐 + 𝑑 &~: ㄜ+ㄑ :=: ㄑ+ㄜ) %@?= (𝑏 + 𝑎 + 𝑑 + 𝑐 :: Expr)
     , testCase "𝑎 + 𝑏 + 𝑐 + 𝑑  &~:  𝑏+𝑐 :=: 𝑐+𝑏" $
      (𝑎 + 𝑏 + 𝑐 + 𝑑 &~: 𝑏+𝑐 :=: 𝑐+𝑏) %@?= (𝑎 + 𝑐 + 𝑏 + 𝑑 :: Expr)
     , testCase "𝑎 + 𝑏 + 𝑐 + 𝑑  &~?  ㄜ+ㄑ :=: ㄑ+ㄜ" $
      (𝑎 + 𝑏 + 𝑐 + 𝑑 &~? ㄜ+ㄑ :=: ㄑ+ㄜ) @?= [ 𝑏 + 𝑎 + 𝑐 + 𝑑
                                              , 𝑎 + 𝑐 + 𝑏 + 𝑑
                                              , 𝑎 + 𝑏 + 𝑑 + 𝑐 :: Expr]
     , testCase "𝑎 + 𝑐 + 𝑏  &~?  𝑎+𝑐 :=: ξ" $
      (𝑎 + 𝑐 + 𝑏 &~? 𝑎+𝑐 :=: ξ ) @?= [ ξ + 𝑏 :: Expr]
     , testCase "𝑎 + 𝑏 + 𝑐  &~?  𝑏+𝑐 :=: 𝑐+𝑏  &~? 𝑎+𝑐 :=: ξ" $
      ((𝑎 + 𝑏 + 𝑐  &~? 𝑏+𝑐:=:𝑐+𝑏) >>= (&~? 𝑎+𝑐:=:ξ) )
               @?= [ ξ+𝑏 :: Expr]
     , testCase "𝑎*𝑥 + 𝑏*𝑥 + 𝑐  &~: ㄏ*ㄘ+ㄐ*ㄘ :=: (ㄏ+ㄐ)*ㄘ" $
      (𝑎*𝑥 + 𝑏*𝑥 + 𝑐 &~: ㄏ*ㄘ+ㄐ*ㄘ :=: (ㄏ+ㄐ)*ㄘ) %@?= ((𝑎+𝑏)*𝑥 + 𝑐 :: Expr)
     , testCase "(𝑎+𝑏)*𝑥 + 𝑐  &~: (ㄏ+ㄐ)*ㄘ :=: ㄏ*ㄘ+ㄐ*ㄘ" $
      ((𝑎+𝑏)*𝑥 + 𝑐 &~: (ㄏ+ㄐ)*ㄘ :=: ㄏ*ㄘ+ㄐ*ㄘ) %@?= (𝑎*𝑥 + 𝑏*𝑥 + 𝑐 :: Expr)
     , testCase "𝑎*𝑏*𝑐*𝑑 &~: 𝑎*𝑏 :=: 𝑏*𝑎" $
      (𝑎*𝑏*𝑐*𝑑  &~: 𝑎*𝑏 :=: 𝑏*𝑎) %@?= (𝑏*𝑎*𝑐*𝑑 :: Expr)
     , testCase "𝑎*𝑏*𝑐*𝑑 &~: 𝑏*𝑐 :=: 𝑐*𝑏" $
      (𝑎*𝑏*𝑐*𝑑  &~: 𝑏*𝑐 :=: 𝑐*𝑏) %@?= (𝑎*𝑐*𝑏*𝑑 :: Expr)
     , testCase "𝑎*𝑏*𝑐*𝑑 &~: 𝑐*𝑑 :=: 𝑑*𝑐" $
      (𝑎*𝑏*𝑐*𝑑  &~: 𝑐*𝑑 :=: 𝑑*𝑐) %@?= (𝑎*𝑏*𝑑*𝑐 :: Expr)
     , testCase "𝑎 + 𝑏 - 𝑐 &~: 𝑏-𝑐 :=: (-𝑐)+𝑏" $
      (𝑎 + 𝑏 - 𝑐 &~: 𝑏-𝑐 :=: (-𝑐)+𝑏) %@?= (𝑎 + (-𝑐) + 𝑏 :: Expr)
     , testCase "Rename local symbols" $
      (map succ%$> 𝑎+𝑝) * 𝑥  %@?=  ((𝑏+𝑞) * 𝑥 :: Expr)
     ]
  , testGroup "Show instance"
     [ testCase "𝑎+𝑏+𝑐" $
      𝑎+𝑏+𝑐 %@?= "𝑎+𝑏+𝑐"
     , testCase "𝑎-𝑏+𝑐" $
      𝑎-𝑏+𝑐 %@?= "𝑎-𝑏+𝑐"
     , testCase "𝑎+(𝑏+𝑐)" $
      𝑎+(𝑏+𝑐) %@?= "𝑎+(𝑏+𝑐)"
     , testCase "𝑎+𝑏*𝑐" $
      𝑎+𝑏*𝑐 %@?= "𝑎+𝑏*𝑐"
     , testCase "3*𝑧-1" $
      3*𝑧-1 %@?= "3*𝑧-1"
     , testCase "(𝑎+𝑏)*𝑐" $
      (𝑎+𝑏)*𝑐 %@?= "(𝑎+𝑏)*𝑐"
     , testCase "abs (𝑎+𝑏)" $
      abs (𝑎+𝑏) %@?= "abs (𝑎+𝑏)"
     , testCase "abs 3" $
      abs 3 %@?= "abs 3"
     , testCase "𝑎 + -3" $
      𝑎+(-3) %@?= "𝑎-3"
     , testCase "𝑎 / signum π" $
      𝑎/signum π %@?= "𝑎/signum π"
     , testCase "logBase 2 32 ** atan pi" $
      logBase 2 32 ** atan pi %@?= "2`logBase`32**atan pi"
     , testCase "37.84" $
      37.84 %@?= "37.84"
     , testCase "5e-23" $
      5e-23 %@?= "5e-23"
     , testCase "-5.3e7" $
      -5.3e8 %@?= " -5.3e8"
     ]
  ]

infix 1 %@?=
class ComparableExpressions e f | f -> e where
  (%@?=) :: HasCallStack => e -> f -> Assertion
  

instance ComparableExpressions Expr Expr where
  e %@?= f
   | e==f       = return ()
   | otherwise  = assertFailure
                   $ "Expected "++show f++" 『structure: "++showStructure f++"』,"
                     ++ "\nbut got " ++show e++" 『structure: "++showStructure e++"』,"

instance ComparableExpressions Expr String where
  e %@?= f
   | show e==f  = return ()
   | otherwise  = assertFailure $ "Expected \""++f++"\""
                     ++ "\nbut got \"" ++show e++"\" 『structure: "++showStructure e++"』,"
