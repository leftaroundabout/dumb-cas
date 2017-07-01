-- |
-- Module      : CAS.Dumb.Util.These
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

module CAS.Dumb.Util.These where

import qualified Data.Map as Map
import Data.Map (Map)

import GHC.Generics


data These a b = This a
               | That b
               | These a b
  deriving (Functor, Eq, Generic)


traverseUnion :: (Applicative t, Ord k)
            => (These a b -> t c) -> Map k a -> Map k b -> t (Map k c)
traverseUnion f ma mb = traverse f $ Map.unionWith (\(This a) (That b) -> These a b)
                                                   (This<$>ma) (That<$>mb)

traverseUnionConflicts :: (Applicative t, Ord k)
            => (a -> a -> t a) -> Map k a -> Map k a -> t (Map k a)
traverseUnionConflicts f ma mb = traverseUnion f' ma mb
 where f' (This a) = pure a
       f' (That b) = pure b
       f' (These a b) = f a b
