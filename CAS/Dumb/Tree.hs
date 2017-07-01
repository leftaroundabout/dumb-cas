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

import qualified Data.Hashable as SH
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Map as Map
import Data.Map (Map)

import GHC.Generics


data CAS s² s¹ s⁰ = Symbol !s⁰
                  | Function !s¹ (CAS s² s¹ s⁰)
                  | Operator !s² (CAS s² s¹ s⁰) (CAS s² s¹ s⁰)
  deriving (Functor, Eq, Generic)

instance (SH.Hashable s⁰, SH.Hashable s¹, SH.Hashable s²)
              => SH.Hashable (CAS s² s¹ s⁰)


data Equality s² s¹ s⁰ = Equality {
   originalExpression :: !(CAS s² s¹ s⁰)
 , transformationOptions :: [Equality s² s¹ s⁰]
 }

type GapId = Int
data GapOr s = Gap GapId
             | NoGap s deriving (Eq, Functor, Generic)

type Expattern s² s¹ s⁰ = Equality s² s¹ (GapOr s⁰)

matchPattern :: CAS s² s¹ (GapOr s⁰) -> CAS s² s¹ s⁰ -> Maybe (Map GapId (CAS s² s¹ s⁰))
matchPattern (Symbol (Gap i)) e = Just $ Map.singleton i e

infixl 1 &==
(&==) :: (Eq s⁰, Eq s¹, Eq s²) => CAS s² s¹ s⁰ -> Expattern s² s¹ s⁰ -> CAS s² s¹ s⁰
e &== Equality orig (Equality alt _:_)
  | Just varMatches <- matchPattern orig e
      = case fillGaps varMatches alt of
          Just refilled -> refilled
e &== _ = e

fillGaps :: Map GapId (CAS s² s¹ s⁰) -> (CAS s² s¹ (GapOr s⁰)) -> Maybe (CAS s² s¹ s⁰)
fillGaps matches (Symbol (Gap i))
  | rematch@(Just _) <- Map.lookup i matches  = rematch

exploreEquality :: (Eq s⁰, Eq s¹, Eq s²)
           => [Expattern s² s¹ s⁰] -> CAS s² s¹ s⁰ -> Equality s² s¹ s⁰
exploreEquality tfms = undefined
