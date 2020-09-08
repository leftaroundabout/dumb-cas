-- |
-- Module      : CAS.Dumb.Symbols
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
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
import GHC.Stack (HasCallStack)

import Data.Ratio (denominator, numerator)
import Numeric.Literals.Decimal


data SymbolD σ c = NatSymbol !Integer
                 | PrimitiveSymbol Char
                 | StringSymbol c

data Infix s = Infix {
    symbolFixity :: !Hs.Fixity
  , infixSymbox :: !s
  }

instance Eq s => Eq (Infix s) where
  Infix _ o == Infix _ p = o==p

type family SpecialEncapsulation s

data Encapsulation s = Encapsulation {
      needInnerParens, haveOuterparens :: !Bool
    , leftEncaps, rightEncaps :: !s
    }
  | SpecialEncapsulation (SpecialEncapsulation s)

instance Eq (Encapsulation String) where
  Encapsulation _ _ l r == Encapsulation _ _ l' r'
         = dropParens (reverse l) r == dropParens (reverse l') r'
   where dropParens ('(':lr) (')':rr) = dropParens lr rr
         dropParens (' ':lr) rr = dropParens lr rr
         dropParens lr (' ':rr) = dropParens lr rr
         dropParens lr rr = (lr,rr)
  SpecialEncapsulation e == SpecialEncapsulation e' = e==e'
  _ == _ = False

type AlgebraExpr σ l = CAS (Infix l) (Encapsulation l) (SymbolD σ l)
type AlgebraExpr' γ σ l = CAS' γ (Infix l) (Encapsulation l) (SymbolD σ l)
type AlgebraPattern σ l = AlgebraExpr' GapId σ l

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



data AlgebraicInvEncapsulation
       = Negation | Reciprocal
 deriving (Eq, Show)

type instance SpecialEncapsulation String = AlgebraicInvEncapsulation

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ String)
          => Num (AlgebraExpr' γ σ String) where
  fromInteger n
   | n<0        = negate . fromInteger $ -n
   | otherwise  = Symbol $ NatSymbol n
  (+) = chainableInfixL (==plusOp) plusOp
   where fcs = fromCharSymbol ([]::[σ])
         plusOp = Infix (Hs.Fixity 6 Hs.InfixL) $ fcs '+'
  (*) = chainableInfixL (==mulOp) mulOp
   where fcs = fromCharSymbol ([]::[σ])
         mulOp = Infix (Hs.Fixity 7 Hs.InfixL) $ fcs '*'
  abs = symbolFunction "abs "
  signum = symbolFunction "signum "
  negate = Function $ SpecialEncapsulation Negation

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ String)
          => Fractional (AlgebraExpr' γ σ String) where
  fromRational n = case fromRational n of
     n:%d -> fromIntegral n / fromIntegral d
     nSci -> Symbol (StringSymbol $ show nSci)
  recip = Function $ SpecialEncapsulation Reciprocal

instance ∀ σ γ . (SymbolClass σ, SCConstraint σ String)
          => Floating (AlgebraExpr' γ σ String) where
  pi = Symbol $ StringSymbol "pi"
  (**) = symbolInfix (Infix (Hs.Fixity 8 Hs.InfixL) "**")
  logBase = symbolInfix (Infix (Hs.Fixity 10 Hs.InfixL) "`logBase`")
  sqrt = symbolFunction $ "sqrt "
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


class Eq (SpecialEncapsulation c) => RenderableEncapsulations c where
  fixateAlgebraEncaps :: (SymbolClass σ, SCConstraint σ c)
      => CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c)
                         -> CAS' γ (Infix c) (Encapsulation c) (SymbolD σ c)

instance RenderableEncapsulations String where
  fixateAlgebraEncaps (OperatorChain x
                         ((o,Function (SpecialEncapsulation ι) z):ys))
     | (Infix (Hs.Fixity 6 Hs.InfixL) "+", Negation) <- (o,ι)
           = case fixateAlgebraEncaps $ OperatorChain x ys of
               x' -> Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-") x' z'
     | (Infix (Hs.Fixity 7 Hs.InfixL) "*", Reciprocal) <- (o,ι)
           = case fixateAlgebraEncaps $ OperatorChain x ys of
               x' -> Operator (Infix (Hs.Fixity 7 Hs.InfixL) "/") x' z'
   where z' = fixateAlgebraEncaps z
  fixateAlgebraEncaps (OperatorChain x []) = fixateAlgebraEncaps x
  fixateAlgebraEncaps (OperatorChain x ((o@(Infix (Hs.Fixity _ Hs.InfixL) _), z):ys))
      = Operator o (fixateAlgebraEncaps $ OperatorChain x ys) (fixateAlgebraEncaps z)
  fixateAlgebraEncaps (Operator o x (Function (SpecialEncapsulation ι) y))
     | (Infix (Hs.Fixity 6 Hs.InfixL) "+", Negation) <- (o,ι)
           = Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-") x' y'
     | (Infix (Hs.Fixity 7 Hs.InfixL) "*", Reciprocal) <- (o,ι)
           = Operator (Infix (Hs.Fixity 7 Hs.InfixL) "/") x' y'
   where [x',y'] = fixateAlgebraEncaps<$>[x,y]
  fixateAlgebraEncaps (Function (SpecialEncapsulation Negation) e)
            = Operator (Infix (Hs.Fixity 6 Hs.InfixL) "-")
                (Symbol $ StringSymbol " ") $ fixateAlgebraEncaps e
  fixateAlgebraEncaps (Function (SpecialEncapsulation Reciprocal) e)
            = Operator (Infix (Hs.Fixity 7 Hs.InfixL) "/")
                (Symbol $ NatSymbol 1) $ fixateAlgebraEncaps e
  fixateAlgebraEncaps (Function f e) = Function f $ fixateAlgebraEncaps e
  fixateAlgebraEncaps (Operator o x y)
        = Operator o (fixateAlgebraEncaps x) (fixateAlgebraEncaps y)
  fixateAlgebraEncaps (OperatorChain x₀ oys)
        = OperatorChain (fixateAlgebraEncaps x₀) (second fixateAlgebraEncaps <$> oys)
  fixateAlgebraEncaps e = e

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

expressionFixity :: AlgebraExpr σ c -> Maybe Hs.Fixity
expressionFixity (Symbol _) = Nothing
expressionFixity (Function _ _) = Nothing
expressionFixity (Operator (Infix fxty _) _ _) = Just fxty
expressionFixity (OperatorChain _ ((Infix fxty _,_):_)) = Just fxty
expressionFixity (OperatorChain x₀ []) = expressionFixity x₀
expressionFixity (Gap _) = Nothing

renderSymbolExpression :: ∀ σ c r . (SymbolClass σ, SCConstraint σ c, HasCallStack)
         => ContextFixity -> RenderingCombinator σ c r
                    -> AlgebraExpr σ c -> r
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
renderSymbolExpression ctxt ρ (OperatorChain x []) = renderSymbolExpression ctxt ρ x
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
renderSymbolExpression _ _ (Function (SpecialEncapsulation _) _) = error
 "`renderSymbolExpression` cannot handle `SpecialEncapsulation`; please pre-process accordingly."


showsPrecASCIISymbol :: (ASCIISymbols c, SymbolClass σ, SCConstraint σ c)
       => Int -> AlgebraExpr σ c -> ShowS
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
       => Int -> AlgebraExpr σ c -> ShowS
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
-- 
--   Note that this can /not/ be used with number literals.
(%$>) :: ∀ σ c c' γ s² s¹ . (SymbolClass σ, SCConstraint σ c)
         => (c -> c') -> CAS' γ s² s¹ (SymbolD σ c) -> CAS' γ s² s¹ (SymbolD σ c')
f %$> Symbol (PrimitiveSymbol c) = case fromCharSymbol ([]::[σ]) of
         fcs -> Symbol . StringSymbol . f $ fcs c
f %$> Symbol (StringSymbol s) = Symbol . StringSymbol $ f s
_ %$> Symbol (NatSymbol _) = error "`%$>` cannot be used with number literals."
f %$> Function g q = Function g $ f %$> q
f %$> Operator o p q = Operator o (f%$>p) (f%$>q)
f %$> OperatorChain p qs = OperatorChain (f%$>p) (second (f%$>)<$>qs)
f %$> Gap γ = Gap γ



continueExpr :: (Eq l, Monoid l)
     => ( AlgebraExpr' γ σ l -> AlgebraExpr' γ σ l -> AlgebraExpr' γ σ l )
       -- ^ Combinator to use for chaining the new expression to the old ones
     -> ( AlgebraExpr' γ σ l -> AlgebraExpr' γ σ l )
       -- ^ Transformation to apply to the rightmost expression in the previous chain
     -> ( AlgebraExpr' γ σ l -> AlgebraExpr' γ σ l )
       -- ^ Transformation which appends the result.
continueExpr op f = go
 where go (OperatorChain e₀ ((eo@(Infix (Hs.Fixity fte _) _), eΩ):es))
         | fte <= chainingFxty
                    = associativeOperator eo (OperatorChain e₀ es) (go eΩ)
       go e
         | Just (co, fxtyDir) <- chainingOp
              = OperatorChain e [(Infix (Hs.Fixity chainingFxty fxtyDir) co, f e)]
         | otherwise
              = op e $ f e
       (chainingFxty, chainingOp)
                      = case op (Symbol $ StringSymbol mempty)
                                (Symbol $ StringSymbol mempty) of
          OperatorChain _ ((Infix (Hs.Fixity fxty fxtyDir) op, _):_)
            -> (fxty, Just (op, fxtyDir))
          _ -> (-1, Nothing)



infixl 1 &~~!, &~~:

-- | Apply a sequence of pattern-transformations and yield the result
--   concatenated to the original via the corresponding chain-operator.
--   Because only the rightmost expression in a chain is processed,
--   this can be iterated, giving a chain of intermediate results.
--
--   If one of the patterns does not match, this manipulator will raise
--   an error.
(&~~!) :: ( Eq l, Eq (Encapsulation l), SymbolClass σ, SCConstraint σ l
         , Show (AlgebraExpr σ l), Show (AlgebraPattern σ l) )
    => AlgebraExpr σ l -> [AlgebraPattern σ l] -> AlgebraExpr σ l
e &~~! [] = e
OperatorChain e₀ ((eo@(Infix (Hs.Fixity fte _) _), eΩ):es)
     &~~! tfms@(OperatorChain p₀ [(to@(Infix (Hs.Fixity ftp _) _),p₁)] : _)
   | fte<=ftp   = associativeOperator eo (OperatorChain e₀ es) (eΩ&~~!tfms)
e &~~! tfms@(OperatorChain _ [(tfmOp, _)] : _)
  = OperatorChain e [(tfmOp, go e tfms)]
 where go e' (OperatorChain p₀ [(tfmOp', p₁)] : tfms') = go (e' &~! (p₀:=:p₁)) tfms'
       go e' [] = e'


-- | Apply a sequence of pattern-transformations, each in every spot possible,
--   and yield the result
--   concatenated to the original via the corresponding chain-operator.
--   Because only the rightmost expression in a chain is processed,
--   this can be iterated, giving a chain of intermediate results.
(&~~:) :: ( Eq l, Eq (Encapsulation l), SymbolClass σ, SCConstraint σ l
         , Show (AlgebraExpr σ l), Show (AlgebraPattern σ l) )
    => AlgebraExpr σ l -> [AlgebraPattern σ l] -> AlgebraExpr σ l
e &~~: [] = e
OperatorChain e₀ ((eo@(Infix (Hs.Fixity fte _) _), eΩ):es)
     &~~: tfms@(OperatorChain p₀ [(to@(Infix (Hs.Fixity ftp _) _),p₁)] : _)
   | fte<=ftp   = associativeOperator eo (OperatorChain e₀ es) (eΩ&~~:tfms)
e &~~: tfms@(OperatorChain _ [(tfmOp, _)] : _)
  = OperatorChain e [(tfmOp, go e tfms)]
 where go e' (OperatorChain p₀ [(tfmOp', p₁)] : tfms')
          = case e' &~: (p₀:=:p₁) of
              alt -> go alt tfms'
       go e' [] = e'
