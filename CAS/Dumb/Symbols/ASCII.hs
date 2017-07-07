{-# LANGUAGE TemplateHaskell       #-}

module CAS.Dumb.Symbols.ASCII () where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols.PatternGenerator

data Symbol = StringSymbol String
type Expression' γ s² s¹ = CAS' γ s² s¹ Symbol

mkUppercaseSymbols ''Expression' ['A'..'Z']

