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

data Symbol = NatSymbol !Integer
            | StringSymbol String

data Infix s = Infix {
    symbolFixity :: !Hs.Fixity
  , infixSymbox :: !s
  }
type InfixSymbol = Infix String

data Encapsulation s = Encapsulation {
      leftEncaps, rightEncaps :: !s }

type SEncapsulation = Encapsulation String

parenthesise :: SEncapsulation
parenthesise = Encapsulation "(" ")"
      
symbolInfix :: Infix s² -> s¹
  -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰
symbolInfix infx encaps a b = Operator infx (Function encaps a) (Function encaps b)

symbolFunction :: Monoid s¹ => s¹ -> Encapsulation s¹
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
symbolFunction f (Encapsulation l r) a = Function (Encapsulation (f<>l) r) a

instance Num (CAS' γ InfixSymbol SEncapsulation Symbol) where
  fromInteger = Symbol . NatSymbol
  (+) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) "+") parenthesise
  (*) = symbolInfix (Infix (Hs.Fixity 7 Hs.InfixL) "*") parenthesise
  (-) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) "-") parenthesise
  abs = symbolFunction "abs " parenthesise
  signum = symbolFunction "signum " parenthesise
  negate = symbolFunction "negate " parenthesise

showsPrecASCIISymbol :: Int -> CAS InfixSymbol SEncapsulation Symbol -> ShowS
showsPrecASCIISymbol _ (Symbol (StringSymbol s)) = (s++)
showsPrecASCIISymbol _ (Symbol (NatSymbol n)) = shows n
showsPrecASCIISymbol _ (Function (Encapsulation l r) s)
    = (l++) . showsPrecASCIISymbol 0 s . (r++)
showsPrecASCIISymbol p (Operator (Infix (Hs.Fixity fxty _) fx) a b)
      = showParen (p>=fxty) $ showsPrecASCIISymbol 0 a . (fx++) . showsPrecASCIISymbol 0 b
