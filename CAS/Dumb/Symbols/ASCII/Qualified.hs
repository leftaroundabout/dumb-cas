-- |
-- Module      : CAS.Dumb.Symbols.ASCII.Qualified
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
-- Single-letter variable symbols.
--
-- Defining such variables on the top level, while convenient for brevity, is a bit
-- troublesome because such are often used as local variables in Haskell code. It is
-- recommended to use "CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps"
-- instead of this module.

{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}

module CAS.Dumb.Symbols.ASCII.Qualified (
          module CAS.Dumb.Symbols
        , Symbol, Expression, Pattern
        -- * “Constant variable” symbols
        -- ** Lowercase letters
        ,syma,symb,symc,symd,syme,symf,symg,symh,symi,symj,symk,syml,symm,symn,symo,symp,symq,symr,syms,symt,symu,symv,symw,symx,symy,symz
        -- ** Uppercase letters
        -- $uppercaseCaveat
        ,symA,symB,symC,symD,symE,symF,symG,symH,symI,symJ,symK,symL,symM,symN,symO,symP,symQ,symR,symS,symT,symU,symV,symW,symX,symY,symZ
        -- * Auxiliary
        , Expression'
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols
import CAS.Dumb.Symbols.PatternGenerator
import CAS.Dumb.Symbols.ASCII
    (Symbol, Expression', Expression, Pattern)

import Data.Void
import Data.Monoid
import Control.Arrow


makeQualifiedSymbols ''Expression' "sym" ['a'..'z']

-- $uppercaseCaveat
-- These are only available in GHC>8.2. The ability to use uppercase letters as variables
-- hinges on a hack using GHC's still recent
-- <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms pattern synonyms> feature.
makeQualifiedSymbols ''Expression' "sym" ['A'..'Z']

