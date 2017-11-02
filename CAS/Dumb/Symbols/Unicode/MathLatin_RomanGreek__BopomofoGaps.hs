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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE CPP                   #-}

module CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps (
          module CAS.Dumb.Symbols
        , Symbol, Expression, Pattern
        -- * “Constant variable” symbols
        -- ** Lowercase letters
        -- $UnicodeMathSymHelp
        -- *** Italic Latin
        , 𝑎,𝑏,𝑐,𝑑,𝑒,𝑓,𝑔,ℎ,𝑖,𝑗,𝑘,𝑙,𝑚,𝑛,𝑜,𝑝,𝑞,𝑟,𝑠,𝑡,𝑢,𝑣,𝑤,𝑥,𝑦,𝑧
        -- *** Bold
        , 𝐚,𝐛,𝐜,𝐝,𝐞,𝐟,𝐠,𝐡,𝐢,𝐣,𝐤,𝐥,𝐦,𝐧,𝐨,𝐩,𝐪,𝐫,𝐬,𝐭,𝐮,𝐯,𝐰,𝐱,𝐲,𝐳
        -- *** Greek
        , α,β,γ,δ,ε,ζ,η,θ,ϑ,ι,κ,λ,μ,ν,ξ,ο,π,ρ,ϱ,σ,ς,τ,υ,ϕ,φ,χ,ψ,ω
        -- ** Uppercase letters
        -- $uppercaseCaveat
#if __GLASGOW_HASKELL__ > 801
        -- *** Italic
        , pattern 𝐴, pattern 𝐵, pattern 𝐶, pattern 𝐷, pattern 𝐸, pattern 𝐹, pattern 𝐺, pattern 𝐻, pattern 𝐼, pattern 𝐽, pattern 𝐾, pattern 𝐿, pattern 𝑀, pattern 𝑁, pattern 𝑂, pattern 𝑃, pattern 𝑄, pattern 𝑅, pattern 𝑆, pattern 𝑇, pattern 𝑈, pattern 𝑉, pattern 𝑊, pattern 𝑋, pattern 𝑌, pattern 𝑍
        -- *** Bold
        , pattern 𝐀, pattern 𝐁, pattern 𝐂, pattern 𝐃, pattern 𝐄, pattern 𝐅, pattern 𝐆, pattern 𝐇, pattern 𝐈, pattern 𝐉, pattern 𝐊, pattern 𝐋, pattern 𝐌, pattern 𝐍, pattern 𝐎, pattern 𝐏, pattern 𝐐, pattern 𝐑, pattern 𝐒, pattern 𝐓, pattern 𝐔, pattern 𝐕, pattern 𝐖, pattern 𝐗, pattern 𝐘, pattern 𝐙
        -- *** Blackboard (LaTeX subset)
        , pattern ℂ, pattern ℍ, pattern ℕ, pattern ℚ, pattern ℝ, pattern ℤ
        -- *** Blackboard (nonstandard)
        , pattern 𝔸, pattern 𝔹, pattern 𝔻, pattern 𝔼, pattern 𝔽, pattern 𝔾, pattern 𝕀, pattern 𝕁, pattern 𝕂, pattern 𝕃, pattern 𝕄, pattern 𝕆, pattern 𝕊, pattern 𝕋, pattern 𝕌, pattern 𝕍, pattern 𝕎, pattern 𝕏, pattern 𝕐
        -- *** Script
        , pattern 𝒜, pattern ℬ, pattern 𝒞, pattern 𝒟, pattern ℰ, pattern ℱ, pattern 𝒢, pattern ℋ, pattern ℐ, pattern 𝒥, pattern 𝒦, pattern ℒ, pattern ℳ, pattern 𝒩, pattern 𝒪, pattern 𝒫, pattern 𝒬, pattern ℛ, pattern 𝒮, pattern 𝒯, pattern 𝒰, pattern 𝒱, pattern 𝒲, pattern 𝒳, pattern 𝒴, pattern 𝒵
        -- *** Calligraphic / bold-script
        , pattern 𝓐, pattern 𝓑, pattern 𝓒, pattern 𝓓, pattern 𝓔, pattern 𝓕, pattern 𝓖, pattern 𝓗, pattern 𝓘, pattern 𝓙, pattern 𝓚, pattern 𝓛, pattern 𝓜, pattern 𝓝, pattern 𝓞, pattern 𝓟, pattern 𝓠, pattern 𝓡, pattern 𝓢, pattern 𝓣, pattern 𝓤, pattern 𝓥, pattern 𝓦, pattern 𝓧, pattern 𝓨, pattern 𝓩
        -- *** Fraktur
        , pattern 𝔄, pattern 𝔅, pattern ℭ, pattern 𝔇, pattern 𝔈, pattern 𝔉, pattern 𝔊, pattern ℌ, pattern ℑ, pattern 𝔍, pattern 𝔎, pattern 𝔏, pattern 𝔐, pattern 𝔑, pattern 𝔒, pattern 𝔓, pattern 𝔔, pattern ℜ, pattern 𝔖, pattern 𝔗, pattern 𝔘, pattern 𝔙, pattern 𝔚, pattern 𝔛, pattern 𝔜
        -- *** Greek (LaTeX subset)
        -- $greekUppercaseLaTeXInfo
        , pattern Γ, pattern Δ, pattern Θ, pattern Λ, pattern Ξ, pattern Π, pattern Σ, pattern Υ, pattern Φ, pattern Ψ, pattern Ω
        -- *** Greek (Latin-lookalike)
        , pattern Α, pattern Β, pattern Ε, pattern Ζ, pattern Η, pattern Ι, pattern Κ, pattern Μ, pattern Ν, pattern Ο, pattern Ρ, pattern Τ, pattern Χ
#endif
        -- * Pattern-matching variable symbols
        -- $BopomofoHelp
        , ㄅ,ㄆ,ㄇ,ㄈ,ㄉ,ㄊ,ㄋ,ㄌ,ㄍ,ㄎ,ㄏ,ㄐ,ㄑ,ㄒ,ㄓ,ㄔ,ㄕ,ㄖ,ㄗ,ㄘ,ㄙ,ㄚ,ㄛ,ㄜ,ㄝ,ㄞ,ㄟ,ㄠ,ㄡ,ㄢ,ㄣ,ㄤ,ㄥ,ㄦ,ㄧ,ㄨ,ㄩ,ㄪ,ㄫ,ㄬ
        -- * Auxiliary
        , Expression'
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols hiding ((&~~!), (&~~:), continueExpr)

import CAS.Dumb.Symbols.PatternGenerator

import Data.Void
import Control.Arrow


data Unicode_MathLatin_RomanGreek__BopomofoGaps
instance SymbolClass Unicode_MathLatin_RomanGreek__BopomofoGaps where
  type SCConstraint Unicode_MathLatin_RomanGreek__BopomofoGaps = UnicodeSymbols
  fromCharSymbol _ = fromUnicodeSymbol

type Symbol = SymbolD Unicode_MathLatin_RomanGreek__BopomofoGaps
type Expression' γ s² s¹ c = CAS' γ s² s¹ (Symbol c)
type Expression c = Expression' Void (Infix c) (Encapsulation c) c
type Pattern c = Expression' GapId (Infix c) (Encapsulation c) c

-- $UnicodeMathSymHelp
-- Unicode mathematical italic letters. Italic is the default way maths symbols appear in
-- e.g. LaTeX-rendered documents, thus it makes sense to use them here.
makeSymbols ''Expression' "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"

makeSymbols ''Expression' ['𝐚'..'𝐳']

makeSymbols ''Expression' "αβγδεζηθϑικλμνξοπρϱσςτυϕφχψω"


-- $uppercaseCaveat
-- These are only available in GHC>8.2. The ability to use uppercase letters as variables
-- hinges on a hack using GHC's still recent
-- <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms pattern synonyms> feature.
--
-- You can use the "CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek.Qualified"
-- module if this causes you any trouble; there, all symbols are prefixed with
-- @sym@ and therefore the uppercase ones are still normal lowercase names
-- in the Haskell code.

#if __GLASGOW_HASKELL__ > 801
makeSymbols ''Expression' ['𝐴'..'𝑍']

makeSymbols ''Expression' ['𝐀'..'𝐙']

makeSymbols ''Expression' "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ"

makeSymbols ''Expression' "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"

makeSymbols ''Expression' ['𝓐'..'𝓩']

makeSymbols ''Expression' "𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜"

-- $greekUppercaseLaTeXInfo
-- These are the uppercase greek letters that don't have latin lookalikes. Only these
-- are supported in LaTeX, so for doing maths it's probably best to stick to this subset.

makeSymbols ''Expression' $ ['Α'..'Ρ']++['Σ'..'Ω']
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

instance UnicodeSymbols c => Show (Expression c) where
  showsPrec = showsPrecUnicodeSymbol
instance ∀ c . UnicodeSymbols c => Show (Pattern c) where
  showsPrec p = showsPrecUnicodeSymbol p . purgeGaps
   where purgeGaps (Symbol s) = Symbol s
         purgeGaps (Function f e) = Function f $ purgeGaps e
         purgeGaps (Operator o x y) = Operator o (purgeGaps x) (purgeGaps y)
         purgeGaps (OperatorChain x ys) = OperatorChain (purgeGaps x) (second purgeGaps<$>ys)
         purgeGaps (Gap gid) = Symbol (PrimitiveSymbol (toEnum gid)) :: Expression c
