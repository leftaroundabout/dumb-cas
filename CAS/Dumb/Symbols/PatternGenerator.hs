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
import Data.Char


makeSymbols :: Name -> [Char] -> DecsQ
makeSymbols casType = fmap concat . mapM mkSymbol
 where mkSymbol c
        | isLower c = return
         [ SigD symbName $ ForallT [PlainTV γ, PlainTV s¹, PlainTV s²] [] typeName
        -- c :: casType γ s² s¹
         , ValD (VarP symbName)
                (NormalB $ InfixE (Just $ ConE 'Symbol)
                                  (VarE '($))
                                  (Just $ ConE 'StringSymbol `AppE` LitE (StringL [c])) )
                []
        -- c = Symbol $ StringSymbol "c"
         ]
#if __GLASGOW_HASKELL__ > 802
        | isUpper c = return
         [ PatSynSigD symbName (ForallT [] [] $ ForallT [] [] typeName)
        -- pattern c :: casType γ s² s¹
         , PatSynD symbName
                   (PrefixPatSyn [])
                   ImplBidir
                   ('Symbol `ConP` ['StringSymbol `ConP` [ListP [LitP $ CharL c]]])
        -- pattern c = Symbol (StringSymbol ['c'])
         ] 
#endif
        | otherwise = error
             $ "Can only make symbols out of lower- or uppercase letters, which '"
                                ++ [c] ++ "' is not."
        where symbName = mkName [c]
              typeName = ConT casType `AppT`  VarT γ `AppT`  VarT s² `AppT` VarT s¹
              [γ,s²,s¹] = mkName <$> ["γ","s²","s¹"]

