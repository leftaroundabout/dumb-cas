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
      
symbolInfix :: Infix s² -- ^ The operator we want to describe
            -> s¹       -- ^ Parenthesization action, in case the operands bind too weakly
  -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰
symbolInfix infx@(Infix (Hs.Fixity fxty fxdir) _) encaps a b
               = Operator infx (secureL a) (secureR b)
 where secureL x@(Operator (Infix (Hs.Fixity lfxty _) _) _ _)
        | lfxty < fxty  = Function encaps x
       secureL x@(Operator (Infix (Hs.Fixity lfxty Hs.InfixL) _) _ _)
        | Hs.InfixL <- fxdir
        , lfxty==fxty   = x
       secureL x@(Operator (Infix (Hs.Fixity lfxty _) _) _ _)
        | lfxty==fxty  = Function encaps x
       secureL x  = x
       secureR x@(Operator (Infix (Hs.Fixity lfxty _) _) _ _)
        | lfxty > fxty  = x
       secureR x@(Operator (Infix (Hs.Fixity lfxty Hs.InfixR) _) _ _)
        | Hs.InfixR <- fxdir
        , lfxty==fxty   = x
       secureR x@(Operator (Infix (Hs.Fixity lfxty _) _) _ _)
        | lfxty==fxty  = Function encaps x
       secureR x  = x

symbolFunction :: Monoid s¹ => s¹ -> Encapsulation s¹
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
symbolFunction f (Encapsulation l r) a@(Symbol _)
    = Function (Encapsulation f mempty) a
symbolFunction f (Encapsulation l r) a
    = Function (Encapsulation (f<>l) r) a

instance Num (CAS' γ InfixSymbol SEncapsulation Symbol) where
  fromInteger n
   | n<0        = negate . fromInteger $ -n
   | otherwise  = Symbol $ NatSymbol n
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
