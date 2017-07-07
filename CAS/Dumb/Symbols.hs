-- |
-- Module      : CAS.Dumb.Symbols
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE FlexibleInstances         #-}

module CAS.Dumb.Symbols where

import CAS.Dumb.Tree

import Data.Monoid
import qualified Language.Haskell.TH.Syntax as Hs

data SymbolD σ = StringSymbol String

data Infix s = Infix {
    symbolFixity :: !Hs.Fixity
  , infixSymbox :: !s
  }

data Encapsulation s = Encapsulation {
      leftEncaps, rightEncaps :: !s }

symbolInfix :: Infix s² -- ^ The operator we want to describe
            -> s¹       -- ^ Parenthesization action, in case the operands bind too weakly
  -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰
symbolInfix infx@(Infix (Hs.Fixity fxty fxdir) _) encaps a b
               = Operator infx a b

symbolFunction :: Monoid s¹ => s¹ -> Encapsulation s¹
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
symbolFunction f (Encapsulation l r) a
    = undefined

