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
  -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰ -> CAS' γ (Infix s²) s¹ s⁰
symbolInfix infx@(Infix (Hs.Fixity fxty fxdir) _) a b = Operator infx a b

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
  (+) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) "+")
  (*) = symbolInfix (Infix (Hs.Fixity 7 Hs.InfixL) "*")
  (-) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) "-")
  abs = symbolFunction "abs " parenthesise
  signum = symbolFunction "signum " parenthesise
  negate = symbolFunction "negate " parenthesise


class ASCIISymbols c where
  fromASCIISymbol :: Char -> c
  toASCIISymbols :: c -> String

instance ASCIISymbols String where
  fromASCIISymbol = pure
  toASCIISymbols = id


type RenderingCombinator c r = Bool       -- ^ Should the result be parenthesised?
                            -> Maybe r    -- ^ Left context
                            -> c          -- ^ Expression to render
                            -> Maybe r    -- ^ Right context
                            -> r          -- ^ Rendering result

data ContextFixity = AtLHS Hs.Fixity
                   | AtRHS Hs.Fixity
                   | AtFunctionArgument
                   deriving (Eq)

renderSymbolExpression :: ∀ σ c γ r . (SymbolClass σ, SCConstraint σ c)
         => ContextFixity -> RenderingCombinator c r
                    -> CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c) -> r
renderSymbolExpression _ ρ (Symbol (PrimitiveSymbol c)) = case fromCharSymbol ([]::[σ]) of
                              fcs -> ρ False Nothing (fcs c) Nothing
renderSymbolExpression _ ρ (Symbol (StringSymbol s)) = ρ False Nothing s Nothing
renderSymbolExpression ctxt ρ (Function (Encapsulation l r) x)
   = ρ (ctxt==AtFunctionArgument) Nothing l . Just
      $ ρ False (Just $ renderSymbolExpression AtFunctionArgument ρ x) r Nothing
renderSymbolExpression ctxt ρ (Operator (Infix fxty o) x y)
   = ρ parens (Just $ renderSymbolExpression (AtLHS fxty) ρ x)
              o
              (Just $ renderSymbolExpression (AtRHS fxty) ρ y)
 where parens = case ctxt of
         AtFunctionArgument -> True
         AtLHS (Hs.Fixity pfxty _)         | Hs.Fixity lfxty _ <- fxty
                                           , lfxty < pfxty                      -> True
         AtLHS (Hs.Fixity pfxty Hs.InfixL) | Hs.Fixity lfxty Hs.InfixL <- fxty
                                           , lfxty==pfxty                       -> False
         AtLHS (Hs.Fixity pfxty _)         | Hs.Fixity lfxty _ <- fxty
                                           , lfxty==pfxty                       -> True
         AtLHS _                                                                -> False
         AtRHS (Hs.Fixity pfxty _)         | Hs.Fixity lfxty _ <- fxty
                                           , lfxty < pfxty                      -> True
         AtRHS (Hs.Fixity pfxty Hs.InfixR) | Hs.Fixity lfxty Hs.InfixR <- fxty
                                           , lfxty==pfxty                       -> False
         AtRHS (Hs.Fixity pfxty _)         | Hs.Fixity lfxty _ <- fxty
                                           , lfxty==pfxty                       -> True
         AtRHS _                                                                -> False


showsPrecASCIISymbol :: (Show γ, ASCIISymbols c, SymbolClass σ, SCConstraint σ c)
       => Int -> CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c) -> ShowS
showsPrecASCIISymbol ctxt
      = renderSymbolExpression (AtLHS (Hs.Fixity ctxt Hs.InfixN)) ρ
 where ρ dop lctxt sym rctxt
           = showParen dop $ maybe id id lctxt . (toASCIISymbols sym++) . maybe id id rctxt


class UnicodeSymbols c where
  fromUnicodeSymbol :: Char -> c
  toUnicodeSymbols :: c -> String

instance UnicodeSymbols String where
  fromUnicodeSymbol = pure
  toUnicodeSymbols = id


showsPrecUnicodeSymbol :: (Show γ, UnicodeSymbols c, SymbolClass σ, SCConstraint σ c)
       => Int -> CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c) -> ShowS
showsPrecUnicodeSymbol ctxt
      = renderSymbolExpression (AtLHS (Hs.Fixity ctxt Hs.InfixN)) ρ
 where ρ dop lctxt sym rctxt
           = showParen dop $ maybe id id lctxt . (toUnicodeSymbols sym++) . maybe id id rctxt



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
