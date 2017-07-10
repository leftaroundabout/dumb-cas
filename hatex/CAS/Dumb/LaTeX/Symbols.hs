-- |
-- Module      : CAS.Dumb.LaTeX.Symbols
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- Orphan instances, allowing to construct CAS syntax trees
-- with LaTeX symbols.

module CAS.Dumb.LaTeX.Symbols () where

import CAS.Dumb.Symbols

import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath

import qualified Data.Text as Txt
import Data.String (IsString(..))
import Data.Char (isAlpha, isUpper, isLower)
import Data.Tuple (swap)

import qualified Data.HashMap.Strict as Map
import Data.Hashable

import Control.Monad


instance ASCIISymbols LaTeX where
  fromASCIISymbol c
   | isAlpha c  = fromString [c]
  toASCIISymbols (TeXRaw s) = Txt.unpack s

instance UnicodeSymbols LaTeX where
  fromUnicodeSymbol c
   | Just lc <- Map.lookup c mappingFromUnicode  = lc
   | otherwise  = error $ "Unicode symbol '"++[c]++"' not supported in LaTeX expressions."
  toUnicodeSymbols lc
   | Just c <- Map.lookup lc mappingToUnicode    = [c]
   | otherwise  = "{{{"++Txt.unpack(render lc)++"}}}"
  
mappingFromUnicode :: Map.HashMap Char LaTeX
mappingToUnicode :: Map.HashMap LaTeX Char
InvertibleMap mappingFromUnicode mappingToUnicode
   = mapToLaTeXWith id     "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"
                           "abcdefghijklmnopqrstuvwxyz"
 <|> mapToLaTeXWith mathbf ['𝐚'..'𝐳']
                           ['a'..'z']
 <|> fromAssocList (zip
           ['α',  'β', 'γ',  'δ',  'ε',       'ζ', 'η','θ',  'ϑ',     'ι', 'κ',  'λ'   ]
           [alpha,beta,gamma,delta,varepsilon,zeta,eta,theta,vartheta,iota,kappa,lambda])
 <|> fromAssocList (zip
           ['μ','ν','ξ','π','ρ','ϱ',   'σ',  'ς',     'τ','υ',    'ϕ','φ',   'χ','ψ', 'ω' ]
           [mu, nu, xi, pi, rho,varrho,sigma,varsigma,tau,upsilon,phi,varphi,chi,psi,omega])

remapWith :: (a->b) -> [a] -> [a] -> [(a, b)]
remapWith f = zipWith (\lc rc -> (lc, f rc))

mapToLaTeXWith :: (LaTeX->LaTeX) -> [Char] -> [Char] -> InvertibleMap Char LaTeX
mapToLaTeXWith f l r = fromAssocList $ remapWith (f . fromString . pure) l r



data InvertibleMap a b = InvertibleMap {
      fwdMapping :: Map.HashMap a b
    , revMapping :: Map.HashMap b a
    }

fromAssocList :: (Hashable a, Hashable b, Eq a, Eq b)
                 => [(a,b)] -> InvertibleMap a b
fromAssocList assocs = InvertibleMap (Map.fromList assocs) (Map.fromList $ map swap assocs)

infixl 3 <|>
(<|>) :: (Hashable a, Hashable b, Eq a, Eq b)
                 => InvertibleMap a b -> InvertibleMap a b -> InvertibleMap a b
InvertibleMap af ar<|>InvertibleMap bf br
   = InvertibleMap (Map.union af bf) (Map.union ar br)

