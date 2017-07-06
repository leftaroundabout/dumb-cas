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
               (&~:), (&~?)
             -- * Constructing equality axioms
             , pattern (:=:)
             -- * Variable-symbols
             , module CAS.Dumb.Symbols.Unicode.MathItalicLatin_RomanGreek__BopomofoGaps
             ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols.Unicode.MathItalicLatin_RomanGreek__BopomofoGaps


