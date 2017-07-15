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

import Control.Arrow

import Data.String (IsString)

import GHC.Exts (Constraint)

import Data.Ratio (denominator, numerator)


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
      needInnerParens, haveOuterparens :: !Bool
    , leftEncaps, rightEncaps :: !s
    }

instance Eq (Encapsulation String) where
  Encapsulation _ _ l r == Encapsulation _ _ l' r'
         = dropParens (reverse l) r == dropParens (reverse l') r'
   where dropParens ('(':lr) (')':rr) = dropParens lr rr
         dropParens (' ':lr) rr = dropParens lr rr
         dropParens lr (' ':rr) = dropParens lr rr
         dropParens lr rr = (lr,rr)

don'tParenthesise :: Monoid s¹
                  => CAS' γ (Infix s²) (Encapsulation s¹) s⁰
                  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
don'tParenthesise (Symbol s) = Symbol s
don'tParenthesise (Gap γ) = Gap γ
don'tParenthesise (Function (Encapsulation nin _ l r) x)
        = Function (Encapsulation nin True l r) x
don'tParenthesise x = Function (Encapsulation False True mempty mempty) x
      
symbolInfix :: s² -- ^ The operator we want to describe
  -> CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰
symbolInfix = Operator

symbolFunction :: Monoid s¹ => s¹
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
  -> CAS' γ (Infix s²) (Encapsulation s¹) s⁰
symbolFunction f a = Function (Encapsulation True False f mempty) a

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ String)
          => Num (CAS' γ (Infix String) (Encapsulation String) (SymbolD σ String)) where
  fromInteger n
   | n<0        = negate . fromInteger $ -n
   | otherwise  = Symbol $ NatSymbol n
  (+) = chainableInfixL (==plusOp) plusOp
   where fcs = fromCharSymbol ([]::[σ])
         plusOp = Infix (Hs.Fixity 6 Hs.InfixL) $ fcs '+'
  (*) = chainableInfixL (==mulOp) mulOp
   where fcs = fromCharSymbol ([]::[σ])
         mulOp = Infix (Hs.Fixity 7 Hs.InfixL) $ fcs '*'
  (-) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) $ fcs '-')
   where fcs = fromCharSymbol ([]::[σ])
  abs = symbolFunction "abs "
  signum = symbolFunction "signum "
  negate = Operator (Infix (Hs.Fixity 6 Hs.InfixL) $ fcs '-')
             . Symbol $ StringSymbol " "
   where fcs = fromCharSymbol ([]::[σ])

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ String)
          => Fractional (CAS' γ (Infix String) (Encapsulation String) (SymbolD σ String)) where
  fromRational n
   | n < 0                        = negate . fromRational $ -n
   | denominator n `mod` 10 == 0  = undefined
   | otherwise                    = fromInteger (numerator n)
                                    / fromInteger (denominator n)
  (/) = symbolInfix (Infix (Hs.Fixity 7 Hs.InfixL) $ fcs '/')
   where fcs = fromCharSymbol ([]::[σ])

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ String)
          => Floating (CAS' γ (Infix String) (Encapsulation String) (SymbolD σ String)) where
  pi = Symbol $ StringSymbol "pi"
  (**) = symbolInfix (Infix (Hs.Fixity 6 Hs.InfixL) "**")
  logBase = symbolInfix (Infix (Hs.Fixity 10 Hs.InfixL) "`logBase`")
  exp = symbolFunction $ "exp "
  log = symbolFunction $ "log "
  sin = symbolFunction $ "sin "
  cos = symbolFunction $ "cos "
  tan = symbolFunction $ "tan "
  asin = symbolFunction $ "asin "
  acos = symbolFunction $ "acos "
  atan = symbolFunction $ "atan "
  sinh = symbolFunction $ "sinh "
  cosh = symbolFunction $ "cosh "
  tanh = symbolFunction $ "tanh "
  asinh = symbolFunction $ "asinh "
  acosh = symbolFunction $ "acosh "
  atanh = symbolFunction $ "atanh "

class ASCIISymbols c where
  fromASCIISymbol :: Char -> c
  toASCIISymbols :: c -> String

instance ASCIISymbols String where
  fromASCIISymbol = pure
  toASCIISymbols = id


type RenderingCombinator σ c r
        = Bool        -- ^ Should the result be parenthesised?
       -> Maybe r     -- ^ Left context
       -> SymbolD σ c -- ^ Central expression/function/infix to render
       -> Maybe r     -- ^ Right context
       -> r           -- ^ Rendering result

data ContextFixity = AtLHS Hs.Fixity
                   | AtRHS Hs.Fixity
                   | AtFunctionArgument
                   deriving (Eq)

renderSymbolExpression :: ∀ σ c r . (SymbolClass σ, SCConstraint σ c)
         => ContextFixity -> RenderingCombinator σ c r
                    -> CAS (Infix c) (Encapsulation c) (SymbolD σ c) -> r
renderSymbolExpression _ ρ (Symbol s) = ρ False Nothing s Nothing
renderSymbolExpression ctxt ρ (Function (Encapsulation needInnerP atomical l r) x)
   = ρ (not atomical && ctxt==AtFunctionArgument) Nothing (StringSymbol l) . Just
      $ ρ False (Just $ renderSymbolExpression
                          (if needInnerP then AtFunctionArgument
                                         else AtLHS (Hs.Fixity (-1) Hs.InfixN))
                          ρ x)
                (StringSymbol r) Nothing
renderSymbolExpression ctxt ρ (Operator o x y)
    = renderSymbolExpression ctxt ρ $ OperatorChain x [(o,y)]
renderSymbolExpression ctxt ρ (OperatorChain x ys@(_:_)) = go parens x ys
 where fxty = foldr1 ( \f f' -> if f==f'
                  then f
                  else error "All infixes in an OperatorChain must have the same fixity"
                     ) $ symbolFixity . fst <$> ys
       go parens x [(Infix _ o,y)]
             = ρ parens (Just $ renderSymbolExpression (AtLHS fxty) ρ x)
                        (StringSymbol o)
                        (Just $ renderSymbolExpression (AtRHS fxty) ρ y)
       go parens x ((Infix _ o,y):zs)
             = ρ parens (Just $ go False x zs)
                        (StringSymbol o)
                        (Just $ renderSymbolExpression (AtRHS fxty) ρ y)
       parens = case ctxt of
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


showsPrecASCIISymbol :: (ASCIISymbols c, SymbolClass σ, SCConstraint σ c)
       => Int -> CAS (Infix c) (Encapsulation c) (SymbolD σ c) -> ShowS
showsPrecASCIISymbol ctxt
      = renderSymbolExpression (AtLHS (Hs.Fixity ctxt Hs.InfixN)) ρ
 where ρ dop lctxt (StringSymbol sym) rctxt
           = showParen dop $ maybe id id lctxt . (toASCIISymbols sym++) . maybe id id rctxt
       ρ dop lctxt (NatSymbol n) rctxt
           = showParen dop $ maybe id id lctxt . shows n . maybe id id rctxt
       ρ dop lctxt (PrimitiveSymbol c) rctxt
           = showParen dop $ maybe id id lctxt . (c:) . maybe id id rctxt


class UnicodeSymbols c where
  fromUnicodeSymbol :: Char -> c
  toUnicodeSymbols :: c -> String

instance UnicodeSymbols String where
  fromUnicodeSymbol = pure
  toUnicodeSymbols = id


showsPrecUnicodeSymbol :: (UnicodeSymbols c, SymbolClass σ, SCConstraint σ c)
       => Int -> CAS (Infix c) (Encapsulation c) (SymbolD σ c) -> ShowS
showsPrecUnicodeSymbol ctxt
      = renderSymbolExpression (AtLHS (Hs.Fixity ctxt Hs.InfixN)) ρ
 where ρ dop lctxt (StringSymbol sym) rctxt
           = showParen dop $ maybe id id lctxt . (toUnicodeSymbols sym++) . maybe id id rctxt
       ρ dop lctxt (NatSymbol n) rctxt
           = showParen dop $ maybe id id lctxt . shows n . maybe id id rctxt
       ρ dop lctxt (PrimitiveSymbol c) rctxt
           = showParen dop $ maybe id id lctxt . (c:) . maybe id id rctxt



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
f %$> OperatorChain p qs = OperatorChain (f%$>p) (second (f%$>)<$>qs)
f %$> Gap γ = Gap γ
