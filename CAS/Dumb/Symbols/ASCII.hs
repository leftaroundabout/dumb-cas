{-# LANGUAGE TemplateHaskell       #-}

module CAS.Dumb.Symbols.ASCII () where

import CAS.Dumb.Tree
import CAS.Dumb.Symbols.PatternGenerator

type EP g = Bool

templateFoo ''EP ['A'..'Z']

