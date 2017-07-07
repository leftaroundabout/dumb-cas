-- |
-- Module      : CAS.Dumb.Symbols.ASCII
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- Single-letter variable symbols.
--
-- Defining such variables on the top level, while convenient for brevity, is a bit
-- troublesome because such are often used as local variables in Haskell code. It is
-- recommended to use "CAS.Dumb.Symbols.Unicode.MathItalicLatin_RomanGreek__BopomofoGaps"
-- instead of this module.

{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE CPP                   #-}

module CAS.Dumb.Symbols.ASCII (
          module CAS.Dumb.Symbols
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols
import CAS.Dumb.Symbols.PatternGenerator

data ASCII
type Symbol = SymbolD ASCII
type Expression' γ s² s¹ = CAS' γ s² s¹ Symbol

-- $uppercaseCaveat
-- These are only available in GHC>8.2. The ability to use uppercase letters as variables
-- hinges on a hack using GHC's still recent
-- <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms pattern synonyms> feature.
#if __GLASGOW_HASKELL__ > 802
mkUppercaseSymbols ''Expression' ['A'..'Z']
#endif

