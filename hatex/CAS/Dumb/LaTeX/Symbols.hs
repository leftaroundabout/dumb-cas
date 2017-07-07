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

import qualified Data.Map as Map

import Control.Monad


instance ASCIISymbols LaTeX where
  fromASCIISymbol c
   | isAlpha c  = fromString [c]
  toASCIISymbols (TeXRaw s) = Txt.unpack s

infixl 3 <|>
a<|>b = Map.union a b

instance UnicodeSymbols LaTeX where
  fromUnicodeSymbol = \c -> case Map.lookup c mapping of Just lc -> lc
   where mapping = mapToLaTeXWith id     "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧"
                                         "abcdefghijklmnopqrstuvwxyz"
               <|> mapToLaTeXWith mathbf ['𝐚'..'𝐳']
                                         ['a'..'z']
               <|> Map.fromList (zip
           ['α',  'β', 'γ',  'δ',  'ε',       'ζ', 'η','θ',  'ϑ',     'ι', 'κ',  'λ'   ]
           [alpha,beta,gamma,delta,varepsilon,zeta,eta,theta,vartheta,iota,kappa,lambda])
               <|> Map.fromList (zip
           ['μ','ν','ξ','π','ρ','ϱ',   'σ',  'ς',     'τ','υ',    'ϕ','φ',   'χ','ψ', 'ω' ]
           [mu, nu, xi, pi, rho,varrho,sigma,varsigma,tau,upsilon,phi,varphi,chi,psi,omega])
  toUnicodeSymbols (TeXRaw s) = italicise <$> Txt.unpack s
   where italicise c
          | isLower c  = let i = fromEnum c - fromEnum 'a'
                         in toEnum $ fromEnum '𝑎' + i
          | isUpper c  = let i = fromEnum c - fromEnum 'A'
                         in toEnum $ fromEnum '𝐴' + i

remapWith :: (a->b) -> [a] -> [a] -> [(a, b)]
remapWith f = zipWith (\lc rc -> (lc, f rc))

mapToLaTeXWith :: (LaTeX->LaTeX) -> [Char] -> [Char] -> Map.Map Char LaTeX
mapToLaTeXWith f l r = Map.fromList $ remapWith (f . fromString . pure) l r

