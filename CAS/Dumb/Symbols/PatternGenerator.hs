-- |
-- Module      : CAS.Dumb.Symbols.PatternGenerator
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE CPP                 #-}

module CAS.Dumb.Symbols.PatternGenerator where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols

import Language.Haskell.TH


#if __GLASGOW_HASKELL__ > 802
mkUppercaseSymbols :: Name -> [Char] -> DecsQ
mkUppercaseSymbols casType = fmap concat . mapM mkSymbol
 where mkSymbol c = return
         [ PatSynSigD patName (ForallT [] [] . ForallT [] []
             $ ConT casType `AppT` γ `AppT` s² `AppT` s¹
             )
        -- pattern patName :: casType γ s² s¹
         , PatSynD patName
                   (PrefixPatSyn [])
                   ImplBidir
                   ('Symbol `ConP` ['StringSymbol `ConP` [ListP [LitP $ CharL c]]])
        -- pattern patName = Symbol (StringSymbol 'c')
         ] 
        where patName = mkName [c]
              [γ,s²,s¹] = VarT . mkName <$> ["γ","s²","s¹"]
#endif

