-- |
-- Module      : CAS.Dumb.Symbols.ASCII
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE FlexibleInstances     #-}

module CAS.Dumb.Symbols.ASCII where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols

a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z :: CAS' γ s² s¹ Symbol
[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
    = Symbol . StringSymbol . pure <$> ['a'..'z']


instance Show (CAS InfixSymbol SEncapsulation Symbol) where
  showsPrec = showsPrecASCIISymbol
