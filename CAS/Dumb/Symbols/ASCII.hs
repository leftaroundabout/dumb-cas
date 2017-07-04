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

module CAS.Dumb.Symbols.ASCII (
          module CAS.Dumb.Symbols
        , Symbol
        -- * “Constant variable” symbols
        , a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
        -- * Pattern-matching variable symbols
        , _a,_b,_c,_d,_e,_f,_g,_h,_i,_j,_k,_l,_m,_n,_o,_p,_q,_r,_s,_t,_u,_v,_w,_x,_y,_z
        ) where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols

data ASCII
type Symbol = SymbolD ASCII

a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z :: CAS' γ s² s¹ Symbol
[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
    = Symbol . StringSymbol . pure <$> ['a'..'z']

_a,_b,_c,_d,_e,_f,_g,_h,_i,_j,_k,_l,_m,_n,_o,_p,_q,_r,_s,_t,_u,_v,_w,_x,_y,_z
    :: CAS' GapId s² s¹ s⁰
[_a,_b,_c,_d,_e,_f,_g,_h,_i,_j,_k,_l,_m,_n,_o,_p,_q,_r,_s,_t,_u,_v,_w,_x,_y,_z]
    = Gap . fromEnum <$> ['a'..'z']


instance Show (CAS InfixSymbol SEncapsulation Symbol) where
  showsPrec = showsPrecASCIISymbol
instance Show (CAS' GapId InfixSymbol SEncapsulation Symbol) where
  showsPrec p = showsPrecASCIISymbol p . purgeGaps
   where purgeGaps (Symbol s) = Symbol s
         purgeGaps (Function f e) = Function f $ purgeGaps e
         purgeGaps (Operator o x y) = Operator o (purgeGaps x) (purgeGaps y)
         purgeGaps (Gap gid) = Symbol (StringSymbol ['_',toEnum gid])
                                          :: (CAS InfixSymbol SEncapsulation Symbol)
