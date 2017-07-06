-- |
-- Module      : CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- This module contains a collection of symbols that should be sufficient for usage
-- in most algebra applications. It avoids polluting the namespace with single-letter
-- variables (which are often used as local variables, leading to shadowing issues),
-- by replacing also the Latin letters with less common Unicode symbols. If you're
-- not concerned with this and prefer symbols that can directly be entered on any
-- Western keyboard, use the "CAS.Dumb.Symbols.ASCII" module instead.

{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE CPP                   #-}

module CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps (
          module CAS.Dumb.Symbols
        , Symbol, Expression, Pattern
        -- * “Constant variable” symbols
        -- ** Lowercase letters
        -- $UnicodeMathSymHelp
        , 𝑎,𝑏,𝑐,𝑑,𝑒,𝑓,𝑔,ℎ,𝑖,𝑗,𝑘,𝑙,𝑚,𝑛,𝑜,𝑝,𝑞,𝑟,𝑠,𝑡,𝑢,𝑣,𝑤,𝑥,𝑦,𝑧
        , α,β,γ,δ,ε,ζ,η,θ,ϑ,ι,κ,λ,μ,ν,ξ,ο,π,ρ,ϱ,σ,ς,τ,υ,ϕ,φ,χ,ψ,ω
        -- ** Uppercase letters
        -- $uppercaseCaveat
#if __GLASGOW_HASKELL__ > 802
        , pattern 𝐴, pattern 𝐵, pattern 𝐶, pattern 𝐷, pattern 𝐸, pattern 𝐹, pattern 𝐺, pattern 𝐻, pattern 𝐼, pattern 𝐽, pattern 𝐾, pattern 𝐿, pattern 𝑀, pattern 𝑁, pattern 𝑂, pattern 𝑃, pattern 𝑄, pattern 𝑅, pattern 𝑆, pattern 𝑇, pattern 𝑈, pattern 𝑉, pattern 𝑊, pattern 𝑋, pattern 𝑌, pattern 𝑍
#endif
        -- * Pattern-matching variable symbols
        -- $BopomofoHelp
        , ㄅ,ㄆ,ㄇ,ㄈ,ㄉ,ㄊ,ㄋ,ㄌ,ㄍ,ㄎ,ㄏ,ㄐ,ㄑ,ㄒ,ㄓ,ㄔ,ㄕ,ㄖ,ㄗ,ㄘ,ㄙ,ㄚ,ㄛ,ㄜ,ㄝ,ㄞ,ㄟ,ㄠ,ㄡ,ㄢ,ㄣ,ㄤ,ㄥ,ㄦ,ㄧ,ㄨ,ㄩ,ㄪ,ㄫ,ㄬ
        -- * Auxiliary
        , Expression'
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols

import CAS.Dumb.Symbols.PatternGenerator

import Data.Void


data Unicode_MathLatin_RomanGreek__BopomofoGaps
type Symbol = SymbolD Unicode_MathLatin_RomanGreek__BopomofoGaps
type Expression' γ s² s¹ = CAS' γ s² s¹ (Symbol String)
type Expression = Expression' Void (Infix String) (Encapsulation String)
type Pattern = Expression' GapId (Infix String) (Encapsulation String)

-- $UnicodeMathSymHelp
-- Unicode mathematical italic letters. Italic is the default way maths symbols appear in
-- e.g. LaTeX-rendered documents, thus it makes sense to use them here.
makeSymbols ''Expression' "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"

makeSymbols ''Expression' "αβγδεζηθϑικλμνξοπρϱσςτυϕφχψω"

-- $uppercaseCaveat
-- These are only available in GHC>8.2. The ability to use uppercase letters as variables
-- hinges on a hack using GHC's still recent
-- <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms pattern synonyms> feature.
#if __GLASGOW_HASKELL__ > 802
makeSymbols ''Expression' ['𝐴'..'𝑍']
#endif


-- $BopomofoHelp
-- Using a non-European alphabet such as Bopomofo for 'Gap's (which are always only
-- temporary placeholders that, unlike 'Symbol's, should never appear in any program
-- output) has the advantage of keeping the namespace clean and avoiding ambiguities.
-- 
-- Most of these symbols can easily be entered as
-- <http://vimhelp.appspot.com/digraph.txt.html#Digraphs Vim digraphs>,
-- namely by combining a (latin) letter with the number 4. For instance, @ctrl-k e 4@
-- generates the symbol @ㄜ U+311C BOPOMOFO LETTER E@.
ㄅ,ㄆ,ㄇ,ㄈ,ㄉ,ㄊ,ㄋ,ㄌ,ㄍ,ㄎ,ㄏ,ㄐ,ㄑ,ㄒ,ㄓ,ㄔ,ㄕ,ㄖ,ㄗ,ㄘ,ㄙ,ㄚ,ㄛ,ㄜ,ㄝ,ㄞ,ㄟ
  ,ㄠ,ㄡ,ㄢ,ㄣ,ㄤ,ㄥ,ㄦ,ㄧ,ㄨ,ㄩ,ㄪ,ㄫ,ㄬ:: CAS' GapId s² s¹ s⁰
[ㄅ,ㄆ,ㄇ,ㄈ,ㄉ,ㄊ,ㄋ,ㄌ,ㄍ,ㄎ,ㄏ,ㄐ,ㄑ,ㄒ,ㄓ,ㄔ,ㄕ,ㄖ,ㄗ,ㄘ,ㄙ,ㄚ,ㄛ,ㄜ,ㄝ,ㄞ,ㄟ
  ,ㄠ,ㄡ,ㄢ,ㄣ,ㄤ,ㄥ,ㄦ,ㄧ,ㄨ,ㄩ,ㄪ,ㄫ,ㄬ]
    = Gap . fromEnum <$> ['ㄅ'..'ㄬ']

instance Show Expression where
  showsPrec = showsPrecUnicodeSymbol
instance Show Pattern where
  showsPrec p = showsPrecUnicodeSymbol p . purgeGaps
   where purgeGaps (Symbol s) = Symbol s
         purgeGaps (Function f e) = Function f $ purgeGaps e
         purgeGaps (Operator o x y) = Operator o (purgeGaps x) (purgeGaps y)
         purgeGaps (Gap gid) = Symbol (StringSymbol [toEnum gid]) :: Expression
