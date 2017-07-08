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
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UnicodeSyntax             #-}

module CAS.Dumb.Symbols where

import CAS.Dumb.Tree

import Data.Monoid
import qualified Language.Haskell.TH.Syntax as Hs

import Data.String (IsString)

import GHC.Exts (Constraint)


data SymbolD σ c = NatSymbol !Integer
                 | PrimitiveSymbol Char
                 | StringSymbol c

data Infix s = Infix {
    symbolFixity :: !Hs.Fixity
  , infixSymbox :: !s
  }

instance Eq s => Eq (Infix s) where
  Infix _ o == Infix _ p = o==p

data Encapsulation s = Encapsulation {
      leftEncaps, rightEncaps :: !s }

instance Eq (Encapsulation String) where
  Encapsulation l r == Encapsulation l' r'
         = dropParens (reverse l) r == dropParens (reverse l') r'
   where dropParens ('(':lr) (')':rr) = dropParens lr rr
         dropParens (' ':lr) rr = dropParens lr rr
         dropParens lr (' ':rr) = dropParens lr rr
         dropParens lr rr = (lr,rr)

parenthesise :: IsString s => Encapsulation s
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
symbolFunction f (Encapsulation l r) a@(Gap _)
    = Function (Encapsulation f mempty) a
symbolFunction f (Encapsulation l r) a
    = Function (Encapsulation (f<>l) r) a

instance (Monoid c, IsString c)
          => Num (CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c)) where
  fromInteger n
   | n<0        = negate . fromInteger $ -n
   | otherwise  = Symbol $ NatSymbol n
  (+) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) "+") parenthesise
  (*) = symbolInfix (Infix (Hs.Fixity 7 Hs.InfixL) "*") parenthesise
  (-) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) "-") parenthesise
  abs = symbolFunction "abs " parenthesise
  signum = symbolFunction "signum " parenthesise
  negate = symbolFunction "negate " parenthesise


class ASCIISymbols c where
  fromASCIISymbol :: Char -> c
  toASCIISymbols :: c -> String

instance ASCIISymbols String where
  fromASCIISymbol = pure
  toASCIISymbols = id


showsPrecASCIISymbol :: (Show γ, ASCIISymbols c)
       => Int -> CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c) -> ShowS
showsPrecASCIISymbol _ (Symbol (PrimitiveSymbol c)) = (c:)
showsPrecASCIISymbol _ (Symbol (StringSymbol s)) = (toASCIISymbols s++)
showsPrecASCIISymbol _ (Symbol (NatSymbol n)) = shows n
showsPrecASCIISymbol p (Function (Encapsulation l r) s)
    = showParen (p>9) $ (toASCIISymbols l++) . showsPrecASCIISymbol 0 s . (toASCIISymbols r++)
showsPrecASCIISymbol p (Operator (Infix (Hs.Fixity fxty _) fx) a b)
    = showParen (p>=fxty) $ showsPrecASCIISymbol 0 a . (toASCIISymbols fx++) . showsPrecASCIISymbol 0 b
showsPrecASCIISymbol p (Gap γ)
    = showParen (p>9) $ ("Gap "++) . showsPrec 10 γ

class UnicodeSymbols c where
  fromUnicodeSymbol :: Char -> c
  toUnicodeSymbols :: c -> String

instance UnicodeSymbols String where
  fromUnicodeSymbol = pure
  toUnicodeSymbols = id


showsPrecUnicodeSymbol :: (Show γ, UnicodeSymbols c)
       => Int -> CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c) -> ShowS
showsPrecUnicodeSymbol _ (Symbol (PrimitiveSymbol c)) = (c:)
showsPrecUnicodeSymbol _ (Symbol (StringSymbol s)) = (toUnicodeSymbols s++)
showsPrecUnicodeSymbol _ (Symbol (NatSymbol n)) = shows n
showsPrecUnicodeSymbol p (Function (Encapsulation l r) s)
    = showParen (p>9) $ (toUnicodeSymbols l++) . showsPrecUnicodeSymbol 0 s . (toUnicodeSymbols r++)
showsPrecUnicodeSymbol p (Operator (Infix (Hs.Fixity fxty _) fx) a b)
    = showParen (p>=fxty) $ showsPrecUnicodeSymbol 0 a . (toUnicodeSymbols fx++) . showsPrecUnicodeSymbol 0 b
showsPrecUnicodeSymbol p (Gap γ)
    = showParen (p>9) $ ("Gap "++) . showsPrec 10 γ



class SymbolClass σ where
  type SCConstraint σ :: * -> Constraint
  fromCharSymbol :: (Functor p, SCConstraint σ c) => p σ -> Char -> c

normaliseSymbols :: ∀ σ c γ s² s¹ . (SymbolClass σ, SCConstraint σ c)
                      => CAS' γ s² s¹ (SymbolD σ c) -> CAS' γ s² s¹ (SymbolD σ c)
normaliseSymbols = fmap nmlzSym
 where nmlzSym (PrimitiveSymbol c) = case fromCharSymbol ([]::[σ]) of
           fcs -> StringSymbol $ fcs c
       nmlzSym s = s

instance ∀ σ c . (SymbolClass σ, SCConstraint σ c, Eq c) => Eq (SymbolD σ c) where
  NatSymbol i == NatSymbol j  = i==j
  StringSymbol x == StringSymbol y  = x==y
  PrimitiveSymbol x == PrimitiveSymbol y  = x==y
  x@(PrimitiveSymbol c) == y  = case fromCharSymbol ([]::[σ]) of
            fcs -> StringSymbol (fcs c)==y
  x == y@(PrimitiveSymbol c)  = case fromCharSymbol ([]::[σ]) of
            fcs -> x==StringSymbol (fcs c)
  _ == _ = False

infixl 4 %$>
-- | Transform the symbols of an expression, in their underlying representation.
--
-- @
-- (map succ%$> 𝑎+𝑝) * 𝑥  ≡  (𝑏+𝑞) * 𝑥
-- @
(%$>) :: ∀ σ c c' γ s² s¹ . (SymbolClass σ, SCConstraint σ c)
         => (c -> c') -> CAS' γ s² s¹ (SymbolD σ c) -> CAS' γ s² s¹ (SymbolD σ c')
f %$> Symbol (PrimitiveSymbol c) = case fromCharSymbol ([]::[σ]) of
         fcs -> Symbol . StringSymbol . f $ fcs c
f %$> Symbol (StringSymbol s) = Symbol . StringSymbol $ f s
f %$> Function g q = Function g $ f %$> q
f %$> Operator o p q = Operator o (f%$>p) (f%$>q)
f %$> Gap γ = Gap γ
