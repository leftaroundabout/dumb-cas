module CAS.Dumb.Symbols.PatternGenerator where

import CAS.Dumb.Tree

import Language.Haskell.TH

mkUppercaseSymbols :: Name -> [Char] -> DecsQ
mkUppercaseSymbols _ _ = return []

