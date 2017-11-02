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


makeSymbols :: Name   -- ^ Desired type of the symbols.
            -> [Char] -- ^ The letters you want as symbols.
            -> DecsQ
makeSymbols t = makeQualifiedSymbols t ""

makeQualifiedSymbols
            :: Name   -- ^ Desired type of the symbols.
            -> String -- ^ Prefix for the generated Haskell names.
            -> [Char] -- ^ The letters you want as symbols.
            -> DecsQ
makeQualifiedSymbols casType namePrefix = fmap concat . mapM mkSymbol
 where mkSymbol c
        | isLower (head idfyer) = return
         [ SigD symbName $ ForallT [PlainTV γ, PlainTV s¹, PlainTV s², PlainTV ζ] [] typeName
        -- c :: casType γ s² s¹ ζ
         , ValD (VarP symbName)
                (NormalB . AppE (ConE 'Symbol)
                         . AppE (ConE 'PrimitiveSymbol)
                         $ LitE (CharL c) )
                []
        -- c = Symbol $ StringSymbol "c"
         ]
#if __GLASGOW_HASKELL__ > 801
        | isUpper (head idfyer) = return
         [ PatSynSigD symbName (ForallT [] [] $ ForallT [] [] typeName)
        -- pattern c :: casType γ s² s¹ ζ
         , PatSynD symbName
                   (PrefixPatSyn [])
                   ImplBidir
                   ('Symbol `ConP` ['PrimitiveSymbol `ConP` [LitP $ CharL c]])
        -- pattern c = Symbol (StringSymbol ['c'])
         ] 
#endif
        | otherwise = error
             $ "Can only make symbols out of lower- or uppercase letters, which '"
                                ++ [c] ++ "' is not."
        where idfyer = namePrefix ++ [c]
              symbName = mkName idfyer
              typeName = ConT casType`AppT`VarT γ`AppT`VarT s²`AppT`VarT s¹`AppT`VarT ζ
              [γ,s²,s¹,ζ] = mkName <$> ["γ","s²","s¹","ζ"]

