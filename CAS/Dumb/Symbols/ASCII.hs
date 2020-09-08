-- |
-- Module      : CAS.Dumb.Symbols.ASCII
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

module CAS.Dumb.Symbols.ASCII (
          module CAS.Dumb.Symbols
        , ASCII, Symbol, Expression, Pattern
        -- * “Constant variable” symbols
        -- ** Lowercase letters
        , a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
        -- ** Uppercase letters
        -- $uppercaseCaveat
#if __GLASGOW_HASKELL__ > 801
        , pattern A, pattern B, pattern C, pattern D, pattern E, pattern F, pattern G, pattern H, pattern I, pattern J, pattern K, pattern L, pattern M, pattern N, pattern O, pattern P, pattern Q, pattern R, pattern S, pattern T, pattern U, pattern V, pattern W, pattern X, pattern Y, pattern Z
#endif
        -- * Pattern-matching variable symbols
        , _a,_b,_c,_d,_e,_f,_g,_h,_i,_j,_k,_l,_m,_n,_o,_p,_q,_r,_s,_t,_u,_v,_w,_x,_y,_z
        -- * Auxiliary
        , Expression', ASCIISymbols(..)
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols
import CAS.Dumb.Symbols.PatternGenerator

import Data.Void
import Data.Monoid
import Control.Arrow

data ASCII
instance SymbolClass ASCII where
  type SCConstraint ASCII = ASCIISymbols
  fromCharSymbol _ = fromASCIISymbol

type Symbol = SymbolD ASCII
type Expression' γ s² s¹ c = CAS' γ s² s¹ (Symbol c)
type Expression c = Expression' Void (Infix c) (Encapsulation c) c
type Pattern c = Expression' GapId (Infix c) (Encapsulation c) c


instance Unwieldy c => Unwieldy (Symbol c) where
  unwieldiness (NatSymbol i) = 0.24127 + fromInteger (abs i)
  unwieldiness (PrimitiveSymbol c)
    | c>='a' && c<='z'  = 1.17236 + fromIntegral (fromEnum 'z' - ucp)/49.4530
    | c>='A' && c<='Z'  = 1.17211 + fromIntegral (fromEnum 'Z' - ucp)/49.4571
    | otherwise         = 1.24551 + fromIntegral ucp / 52792.42
                                  + fromIntegral (ucp`mod`136)/9722.3
   where ucp = fromEnum c
  unwieldiness (StringSymbol s) = unwieldiness s

makeSymbols ''Expression' ['a'..'z']

_a,_b,_c,_d,_e,_f,_g,_h,_i,_j,_k,_l,_m,_n,_o,_p,_q,_r,_s,_t,_u,_v,_w,_x,_y,_z
    :: CAS' GapId s² s¹ s⁰
[_a,_b,_c,_d,_e,_f,_g,_h,_i,_j,_k,_l,_m,_n,_o,_p,_q,_r,_s,_t,_u,_v,_w,_x,_y,_z]
    = Gap . fromEnum <$> ['a'..'z']

-- $uppercaseCaveat
-- These are only available in GHC>8.2. The ability to use uppercase letters as variables
-- hinges on a hack using GHC's still recent
-- <https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms pattern synonyms> feature.
--
-- You can use the "CAS.Dumb.Symbols.ASCII.Qualified"
-- module if this causes you any trouble; there, all symbols are prefixed with
-- @sym@ and therefore the uppercase ones are still normal lowercase names
-- in the Haskell code.
#if __GLASGOW_HASKELL__ > 801
makeSymbols ''Expression' ['A'..'Z']
#endif

instance (ASCIISymbols c, RenderableEncapsulations c)
      => Show (CAS (Infix c) (Encapsulation c) (Symbol c)) where
  showsPrec p = showsPrecASCIISymbol p . fixateAlgebraEncaps
   where 
instance ∀ c . (ASCIISymbols c, RenderableEncapsulations c, Monoid c)
       => Show (CAS' GapId (Infix c) (Encapsulation c) (Symbol c)) where
  showsPrec p = showsPrecASCIISymbol p . purgeGaps . fixateAlgebraEncaps
   where purgeGaps (Symbol s) = Symbol s
         purgeGaps (Function f e) = Function f $ purgeGaps e
         purgeGaps (Operator o x y) = Operator o (purgeGaps x) (purgeGaps y)
         purgeGaps (OperatorChain x ys) = OperatorChain (purgeGaps x) (second purgeGaps<$>ys)
         purgeGaps (Gap gid) = Symbol (StringSymbol $ fromASCIISymbol '_'
                                                    <>fromASCIISymbol (toEnum gid) )
                              :: (CAS (Infix c) (Encapsulation c) (Symbol c))
