-- |
-- Module      : CAS.Dumb.Symbols.Unicode.MathItalicLatin_RomanGreek__BopomofoGaps
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

module CAS.Dumb.Symbols.Unicode.MathItalicLatin_RomanGreek__BopomofoGaps (
          module CAS.Dumb.Symbols
        , Symbol
        -- * “Constant variable” symbols
        -- $UnicodeMathSymHelp
        , 𝑎,𝑏,𝑐,𝑑,𝑒,𝑓,𝑔,ℎ,𝑖,𝑗,𝑘,𝑙,𝑚,𝑛,𝑜,𝑝,𝑞,𝑟,𝑠,𝑡,𝑢,𝑣,𝑤,𝑥,𝑦,𝑧
        , α,β,γ,δ,ε,ζ,η,θ,ϑ,ι,κ,λ,μ,ν,ξ,ο,π,ρ,ϱ,σ,ς,τ,υ,ϕ,φ,χ,ψ,ω
        -- * Pattern-matching variable symbols
        -- $BopomofoHelp
        , ㄅ,ㄆ,ㄇ,ㄈ,ㄉ,ㄊ,ㄋ,ㄌ,ㄍ,ㄎ,ㄏ,ㄐ,ㄑ,ㄒ,ㄓ,ㄔ,ㄕ,ㄖ,ㄗ,ㄘ,ㄙ,ㄚ,ㄛ,ㄜ,ㄝ,ㄞ,ㄟ,ㄠ,ㄡ,ㄢ,ㄣ,ㄤ,ㄥ,ㄦ,ㄧ,ㄨ,ㄩ,ㄪ,ㄫ,ㄬ
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols

import CAS.Dumb.Symbols.PatternGenerator

data Unicode_MathItalicLatin_RomanGreek__BopomofoGaps
type Symbol = SymbolD Unicode_MathItalicLatin_RomanGreek__BopomofoGaps
type Expression' γ s² s¹ = CAS' γ s² s¹ Symbol

-- $UnicodeMathSymHelp
-- Unicode mathematical italic letters. Italic is the default way maths symbols appear in
-- e.g. LaTeX-rendered documents, thus it makes sense to use them here.
--
-- Note that the symbols are at runtime /not/ stored in italic form, e.g.
-- @'𝑚' ≡ 'Symbol' ('StringSymbol' "m")@.
𝑎,𝑏,𝑐,𝑑,𝑒,𝑓,𝑔,ℎ,𝑖,𝑗,𝑘,𝑙,𝑚,𝑛,𝑜,𝑝,𝑞,𝑟,𝑠,𝑡,𝑢,𝑣,𝑤,𝑥,𝑦,𝑧 :: Expression' γ s² s¹
[𝑎,𝑏,𝑐,𝑑,𝑒,𝑓,𝑔,ℎ,𝑖,𝑗,𝑘,𝑙,𝑚,𝑛,𝑜,𝑝,𝑞,𝑟,𝑠,𝑡,𝑢,𝑣,𝑤,𝑥,𝑦,𝑧]
    = Symbol . StringSymbol . pure <$> ['a'..'z']

makeSymbols ''Expression' "αβγδεζηθϑικλμνξοπρϱσςτυϕφχψω"


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

instance Show (CAS InfixSymbol SEncapsulation Symbol) where
  showsPrec = showsPrecASCIISymbol
instance Show (CAS' GapId InfixSymbol SEncapsulation Symbol) where
  showsPrec p = showsPrecASCIISymbol p . purgeGaps
   where purgeGaps (Symbol s) = Symbol s
         purgeGaps (Function f e) = Function f $ purgeGaps e
         purgeGaps (Operator o x y) = Operator o (purgeGaps x) (purgeGaps y)
         purgeGaps (Gap gid) = Symbol (StringSymbol [toEnum gid])
                                          :: (CAS InfixSymbol SEncapsulation Symbol)
