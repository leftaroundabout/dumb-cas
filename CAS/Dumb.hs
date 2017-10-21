-- |
-- Module      : CAS.Dumb
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE DeriveFunctor, DeriveGeneric, PatternSynonyms #-}

module CAS.Dumb (
             -- * Symbolic manipulation
               (&~:), (&~?), (&~!), (&~~!), (&~~:), continueExpr
             -- * Constructing equality axioms
             , Equality'((:=:))
             -- * Variable-symbols
             , (%$>)
             , module CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
             -- * Types
             , CAS, CAS', SymbolD, Infix, Encapsulation
             -- * Debugging
             , showStructure, throwStructure
             ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols
import CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
             hiding ((%$>), SymbolD, Encapsulation, Infix)


throwStructure :: CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰
throwStructure = error . showStructure
