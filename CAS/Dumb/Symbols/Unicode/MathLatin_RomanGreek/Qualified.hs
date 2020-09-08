-- |
-- Module      : CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek.Qualified
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
-- This module contains a collection of symbols that should be sufficient for usage
-- in most algebra applications. It is intended as alternative syntax for
-- "CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps", the difference
-- being that the symbol names start with the qualifier @sym@. This means that
-- the uppercase symbols don't need special handling as @PatternSynonyms@.
-- 
-- @
-- 'sym𝑋' ≡ 'CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps.𝑋'
-- @


{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE CPP                   #-}

module CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek.Qualified (
          module CAS.Dumb.Symbols
        , Symbol, Expression, Pattern
        -- * “Constant variable” symbols
        -- ** Lowercase letters
        -- $UnicodeMathSymHelp
        -- *** Italic Latin
        , sym𝑎,sym𝑏,sym𝑐,sym𝑑,sym𝑒,sym𝑓,sym𝑔,symℎ,sym𝑖,sym𝑗,sym𝑘,sym𝑙,sym𝑚,sym𝑛,sym𝑜,sym𝑝,sym𝑞,sym𝑟,sym𝑠,sym𝑡,sym𝑢,sym𝑣,sym𝑤,sym𝑥,sym𝑦,sym𝑧
        -- *** Bold
        , sym𝐚,sym𝐛,sym𝐜,sym𝐝,sym𝐞,sym𝐟,sym𝐠,sym𝐡,sym𝐢,sym𝐣,sym𝐤,sym𝐥,sym𝐦,sym𝐧,sym𝐨,sym𝐩,sym𝐪,sym𝐫,sym𝐬,sym𝐭,sym𝐮,sym𝐯,sym𝐰,sym𝐱,sym𝐲,sym𝐳
        -- *** Greek
        , symα,symβ,symγ,symδ,symε,symζ,symη,symθ,symϑ,symι,symκ,symλ,symμ,symν,symξ,symο,symπ,symρ,symϱ,symσ,symς,symτ,symυ,symϕ,symφ,symχ,symψ,symω
        -- ** Uppercase letters
        -- *** Italic
        ,sym𝐴,sym𝐵,sym𝐶,sym𝐷,sym𝐸,sym𝐹,sym𝐺,sym𝐻,sym𝐼,sym𝐽,sym𝐾,sym𝐿,sym𝑀,sym𝑁,sym𝑂,sym𝑃,sym𝑄,sym𝑅,sym𝑆,sym𝑇,sym𝑈,sym𝑉,sym𝑊,sym𝑋,sym𝑌,sym𝑍
        -- *** Bold
        ,sym𝐀,sym𝐁,sym𝐂,sym𝐃,sym𝐄,sym𝐅,sym𝐆,sym𝐇,sym𝐈,sym𝐉,sym𝐊,sym𝐋,sym𝐌,sym𝐍,sym𝐎,sym𝐏,sym𝐐,sym𝐑,sym𝐒,sym𝐓,sym𝐔,sym𝐕,sym𝐖,sym𝐗,sym𝐘,sym𝐙
        -- *** Blackboard (LaTeX subset)
        ,symℂ,symℍ,symℕ,symℚ,symℝ,symℤ
        -- *** Blackboard (nonstandard)
        ,sym𝔸,sym𝔹,sym𝔻,sym𝔼,sym𝔽,sym𝔾,sym𝕀,sym𝕁,sym𝕂,sym𝕃,sym𝕄,sym𝕆,sym𝕊,sym𝕋,sym𝕌,sym𝕍,sym𝕎,sym𝕏,sym𝕐
        -- *** Script
        ,sym𝒜,symℬ,sym𝒞,sym𝒟,symℰ,symℱ,sym𝒢,symℋ,symℐ,sym𝒥,sym𝒦,symℒ,symℳ,sym𝒩,sym𝒪,sym𝒫,sym𝒬,symℛ,sym𝒮,sym𝒯,sym𝒰,sym𝒱,sym𝒲,sym𝒳,sym𝒴,sym𝒵
        -- *** Calligraphic / bold-script
        ,sym𝓐,sym𝓑,sym𝓒,sym𝓓,sym𝓔,sym𝓕,sym𝓖,sym𝓗,sym𝓘,sym𝓙,sym𝓚,sym𝓛,sym𝓜,sym𝓝,sym𝓞,sym𝓟,sym𝓠,sym𝓡,sym𝓢,sym𝓣,sym𝓤,sym𝓥,sym𝓦,sym𝓧,sym𝓨,sym𝓩
        -- *** Fraktur
        ,sym𝔄,sym𝔅,symℭ,sym𝔇,sym𝔈,sym𝔉,sym𝔊,symℌ,symℑ,sym𝔍,sym𝔎,sym𝔏,sym𝔐,sym𝔑,sym𝔒,sym𝔓,sym𝔔,symℜ,sym𝔖,sym𝔗,sym𝔘,sym𝔙,sym𝔚,sym𝔛,sym𝔜
        -- *** Greek (LaTeX subset)
        -- $greekUppercaseLaTeXInfo
        ,symΓ,symΔ,symΘ,symΛ,symΞ,symΠ,symΣ,symΥ,symΦ,symΨ,symΩ
        -- *** Greek (Latin-lookalike)
        ,symΑ,symΒ,symΕ,symΖ,symΗ,symΙ,symΚ,symΜ,symΝ,symΟ,symΡ,symΤ,symΧ
        -- * Auxiliary
        , Expression'
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols hiding ((&~~!), (&~~:), continueExpr)

import CAS.Dumb.Symbols.PatternGenerator
import CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
            ( Symbol, Expression', Expression, Pattern )

import Data.Void
import Control.Arrow


-- $UnicodeMathSymHelp
-- Unicode mathematical italic letters. Italic is the default way maths symbols appear in
-- e.g. LaTeX-rendered documents, thus it makes sense to use them here.
makeQualifiedSymbols ''Expression' "sym" "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"

makeQualifiedSymbols ''Expression' "sym" ['𝐚'..'𝐳']

makeQualifiedSymbols ''Expression' "sym" "αβγδεζηθϑικλμνξοπρϱσςτυϕφχψω"


makeQualifiedSymbols ''Expression' "sym" ['𝐴'..'𝑍']

makeQualifiedSymbols ''Expression' "sym" ['𝐀'..'𝐙']

makeQualifiedSymbols ''Expression' "sym" "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ"

makeQualifiedSymbols ''Expression' "sym" "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"

makeQualifiedSymbols ''Expression' "sym" ['𝓐'..'𝓩']

makeQualifiedSymbols ''Expression' "sym" "𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜"

-- $greekUppercaseLaTeXInfo
-- These are the uppercase greek letters that don't have latin lookalikes. Only these
-- are supported in LaTeX, so for doing maths it's probably best to stick to this subset.

makeQualifiedSymbols ''Expression' "sym" $ ['Α'..'Ρ']++['Σ'..'Ω']

