-- |
-- Module      : CAS.Dumb.Tree
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module CAS.Dumb.Tree where

import CAS.Dumb.Util.These

import qualified Data.Hashable as SH
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Void
import Control.Monad

import GHC.Generics


data CAS' γ s² s¹ s⁰ = Symbol !s⁰
                     | Function !s¹ (CAS' γ s² s¹ s⁰)
                     | Operator !s² (CAS' γ s² s¹ s⁰) (CAS' γ s² s¹ s⁰)
                     | Gap !γ
  deriving (Functor, Eq, Generic)

type CAS = CAS' Void

instance (SH.Hashable γ, SH.Hashable s⁰, SH.Hashable s¹, SH.Hashable s²)
              => SH.Hashable (CAS' γ s² s¹ s⁰)


data Equality' γ s² s¹ s⁰ = Equality {
   originalExpression :: !(CAS' γ s² s¹ s⁰)
 , transformationOptions :: [Equality' γ s² s¹ s⁰]
 }
type Equality = Equality' Void

type GapId = Int
type Expattern s² s¹ s⁰ = CAS' GapId s² s¹ s⁰
type Eqspattern s² s¹ s⁰ = Equality' GapId s² s¹ s⁰

matchPattern :: (Eq s⁰, Eq s¹, Eq s²)
         => Expattern s² s¹ s⁰ -> CAS s² s¹ s⁰ -> Maybe (Map GapId (CAS s² s¹ s⁰))
matchPattern (Gap i) e = Just $ Map.singleton i e
matchPattern (Symbol s) (Symbol s')
 | s==s'  = Just Map.empty
matchPattern (Function f x) (Function f' ξ)
 | f==f'  = matchPattern x ξ
matchPattern (Operator o x y) (Operator o' ξ υ)
 | o==o'  = do
     xmatches <- matchPattern x ξ
     ymatches <- matchPattern y υ
     traverseUnionConflicts (\v w -> guard (v==w) >> Just v) xmatches ymatches
matchPattern _ _ = Nothing

infixl 1 &==
(&==) :: (Eq s⁰, Eq s¹, Eq s²) => CAS s² s¹ s⁰ -> Eqspattern s² s¹ s⁰ -> CAS s² s¹ s⁰
e &== Equality orig (Equality alt _:_)
  | Just varMatches <- matchPattern orig e
      = case fillGaps varMatches alt of
          Just refilled -> refilled
e &== _ = e

fillGaps :: Map GapId (CAS s² s¹ s⁰) -> (Expattern s² s¹ s⁰) -> Maybe (CAS s² s¹ s⁰)
fillGaps matches (Gap i)
  | rematch@(Just _) <- Map.lookup i matches  = rematch

exploreEquality :: (Eq s⁰, Eq s¹, Eq s²)
           => [Expattern s² s¹ s⁰] -> CAS s² s¹ s⁰ -> Equality s² s¹ s⁰
exploreEquality tfms = undefined
