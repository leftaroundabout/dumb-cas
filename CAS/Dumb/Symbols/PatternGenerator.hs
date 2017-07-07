module CAS.Dumb.Symbols.PatternGenerator where

import CAS.Dumb.Tree

import Language.Haskell.TH

templateFoo :: Name -> [Char] -> DecsQ
templateFoo _ _ = return []

