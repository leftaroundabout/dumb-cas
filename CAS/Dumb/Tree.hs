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

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Void


data CAS' γ s² s¹ s⁰ = Symbol !s⁰
                     | Operator !s² (CAS' γ s² s¹ s⁰) (CAS' γ s² s¹ s⁰)
  deriving (Functor, Eq)

type CAS = CAS' Void


type GapId = Int
type Expattern s² s¹ s⁰ = CAS' GapId s² s¹ s⁰

matchPattern :: Eq s²
         => Expattern s² s¹ s⁰ -> CAS s² s¹ s⁰ -> Maybe (Map GapId (CAS s² s¹ s⁰))
matchPattern (Operator o x y) (Operator o' ξ υ)
 | o==o'  = do
     xmatches <- matchPattern x ξ
     ymatches <- matchPattern y υ
     traverseUnionConflicts (\v w -> Just v) xmatches ymatches
matchPattern _ _ = Nothing


