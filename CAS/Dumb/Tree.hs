module CAS.Dumb.Tree where

import CAS.Dumb.Util.These

import qualified Data.Map as Map
import Data.Map (Map)


data CAS' γ s² s¹ s⁰ = Symbol !s⁰
type Expattern s² s¹ s⁰ = CAS' Int s² s¹ s⁰

matchPattern :: Expattern s² s¹ s⁰ -> CAS' () s² s¹ s⁰ -> Maybe (Map Int (CAS' () s² s¹ s⁰))
matchPattern _ _
   = traverseUnionConflicts undefined undefined undefined


