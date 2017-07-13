-- |
-- Module      : CAS.Dumb.Tree
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE DeriveFunctor, DeriveGeneric       #-}
{-# LANGUAGE TupleSections                      #-}
{-# LANGUAGE PatternSynonyms                    #-}
{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}

module CAS.Dumb.Tree where

import CAS.Dumb.Util.These

import qualified Data.Hashable as SH
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Void
import Control.Monad
import Control.Arrow

import GHC.Generics


data CAS' γ s² s¹ s⁰ = Symbol !s⁰
                     | Function !s¹ (CAS' γ s² s¹ s⁰)
                     | OperatorChain
                          (CAS' γ s² s¹ s⁰)       -- Initial operand
                          [(s², CAS' γ s² s¹ s⁰)] -- Chain of operator-application, in
                                                  -- reverse order (i.e. the head of this
                                                  -- list contains the rightmost operand)
                     | Gap !γ
  deriving (Functor, Eq, Generic)

pattern Operator :: s² -> CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰
pattern Operator o x y = OperatorChain x [(o,y)]


chainableInfixL, chainableInfixR, chainableInfix
               :: (s² -> Bool)  -- ^ Condition that all operators in the chain
                                --   must fulfill to be joined by this one
               -> s²            -- ^ The operator we want to add
               -> CAS' γ s² s¹ s⁰ -> CAS' γ s² s¹ s⁰
               -> CAS' γ s² s¹ s⁰
chainableInfixL ppred infx (OperatorChain x ys) z
 | all (ppred . fst) ys  = OperatorChain x $ (infx,z):ys
chainableInfixL _ infx a b = Operator infx a b

chainableInfixR ppred infx (OperatorChain x ys) z
 | all (ppred . fst) ys  = OperatorChain x $ (infx,z):ys
chainableInfixR ppred infx x (OperatorChain y zs)
 | all (ppred . fst) zs  = OperatorChain x $ zs++[(infx,y)]
chainableInfixR _ infx a b = Operator infx a b

chainableInfix ppred infx (OperatorChain x ys) z
 | all (ppred . fst) ys  = OperatorChain x $ (infx,z):ys
chainableInfix ppred infx x (OperatorChain y zs)
 | all (ppred . fst) zs  = OperatorChain x $ zs++[(infx,y)]
chainableInfix _ infx a b = Operator infx a b



type CAS = CAS' Void

instance (SH.Hashable γ, SH.Hashable s⁰, SH.Hashable s¹, SH.Hashable s²)
              => SH.Hashable (CAS' γ s² s¹ s⁰)


infixr 4 :=, :=:
data Equality' γ s² s¹ s⁰
  = (:=) { originalExpression :: !(CAS' γ s² s¹ s⁰)
         , transformationOptions :: [Equality' γ s² s¹ s⁰] }
  | (:=:) { originalExpression :: !(CAS' γ s² s¹ s⁰)
          , transformedExpression :: !(CAS' γ s² s¹ s⁰) }
type Equality = Equality' Void

type GapId = Int
type Expattern s² s¹ s⁰ = CAS' GapId s² s¹ s⁰
type Eqspattern s² s¹ s⁰ = Equality' GapId s² s¹ s⁰

matchPattern :: ∀ s⁰ s¹ s² . (Eq s⁰, Eq s¹, Eq s²)
         => Expattern s² s¹ s⁰ -> CAS s² s¹ s⁰ -> Maybe (Map GapId (CAS s² s¹ s⁰))
matchPattern (Gap i) e = Just $ Map.singleton i e
matchPattern (Symbol s) (Symbol s')
 | s==s'  = Just Map.empty
matchPattern (Function f x) (Function f' ξ)
 | f==f'  = matchPattern x ξ
matchPattern (OperatorChain x zs) (OperatorChain ξ ζs)
 | (fst<$>zs) == (fst<$>ζs)  = do
     xmatches <- matchPattern x ξ
     zsmatches <- sequence $ zipWith matchPattern (snd<$>zs) (snd<$>ζs)
     merge xmatches zsmatches
 where merge :: Map GapId (CAS s² s¹ s⁰) -> [Map GapId (CAS s² s¹ s⁰)]
                   -> Maybe (Map GapId (CAS s² s¹ s⁰))
       merge acc [] = pure acc
       merge acc (xm:ms)
          = traverseUnionConflicts (\v w -> guard (v==w) >> Just v) xm acc
             >>= (`merge` ms)
matchPattern _ _ = Nothing

infixl 1 &~:, &~?

-- | @expr '&~:' pat ':=:' rep@ replaces every occurence of @pat@ within @expr@ with @rep@.
--
-- For example, <http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119886- 𝑎>·<http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119887- 𝑏> − <http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119888- 𝑐>·<http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119889- 𝑑> '&~:' <http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-12549- ㄅ>·<http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-12568- ㄘ> ':=:' ㄘ·ㄅ yields 𝑏·𝑎 − 𝑑·𝑐.
(&~:) :: (Eq s⁰, Eq s¹, Eq s²) => CAS s² s¹ s⁰ -> Eqspattern s² s¹ s⁰ -> CAS s² s¹ s⁰
e &~: orig := (alt:=_):_ = e &~: orig:=:alt
OperatorChain x ys &~: pat@(OperatorChain ξ υs):=:alt
  | exprLength > patLength
  , (remainSect, patLSect) <- splitAt (exprLength-patLength) ys
    = let (or₀, yr₀) = last remainSect
      in case matchPattern pat (OperatorChain x patLSect) of
       Just varMatchesL -> case ( fillGaps varMatchesL alt
                                , OperatorChain yr₀ (init remainSect) &~: pat:=:alt ) of
          (Just (OperatorChain x' yps'), OperatorChain yr₀' zs')
           | all ((==or₀) . fst) yps'
           , all ((==or₀) . fst) zs'
             -> OperatorChain x' $ zs'++(or₀,yr₀'):yps'
          (Just patReplaced, OperatorChain yr₀' zs')
           | all ((==or₀) . fst) zs'
             -> OperatorChain patReplaced $ zs'++[(or₀,yr₀')]
       Nothing -> let (o₀,y₀) = last ys
                  in case OperatorChain y₀ (init ys) &~: pat:=:alt of
          OperatorChain y₀' yps'
           | all ((==o₀) . fst) yps'
               -> OperatorChain x $ yps'++[(o₀,y₀')]
          patReplaced -> OperatorChain x [(o₀,patReplaced)]
 where patLength = length υs
       exprLength = length ys
e &~: orig:=:alt
  | Just varMatches <- matchPattern orig e
      = case fillGaps varMatches alt of
          Just refilled -> refilled
Function f x &~: p = Function f $ x&~:p
OperatorChain x ys &~: p = OperatorChain (x&~:p) (second (&~:p) <$> ys)
e &~: _ = e

-- | @expr '&~?' pat ':=:' rep@ gives every possible way @pat@ can be replaced exactly
-- once within @expr@.
--
-- For example, <http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119886- 𝑎>·<http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119887- 𝑏> − <http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119888- 𝑐>·<http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-119889- 𝑑> '&~?' <http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-12549- ㄅ>·<http://hackage.haskell.org/package/dumb-cas/docs/CAS-Dumb-Symbols-Unicode-MathLatin_RomanGreek__BopomofoGaps.html#v:-12568- ㄘ> ':=:' ㄘ·ㄅ yields [𝑏·𝑎 − 𝑐·𝑑, 𝑎·𝑏 − 𝑑·𝑐].
(&~?) :: (Eq s⁰, Eq s¹, Eq s²) => CAS s² s¹ s⁰ -> Eqspattern s² s¹ s⁰ -> [CAS s² s¹ s⁰]
e &~? orig := (alt:=_):_ = e &~? orig:=:alt
e &~? orig:=:alt
  | Just varMatches <- matchPattern orig e
      = case fillGaps varMatches alt of
          Just refilled -> [refilled]
Function f x &~? p = Function f <$> (x&~?p)
OperatorChain x [] &~? p = (`OperatorChain`[]) <$> (x&~?p)
OperatorChain x ((o,y):zs) &~? p
       = [OperatorChain ξ ((o,y):ζs) | OperatorChain ξ ζs <- OperatorChain x zs &~? p]
        ++ (OperatorChain x . (:zs) . (o,) <$> (y&~?p))
e &~? _ = []

fillGaps :: Map GapId (CAS s² s¹ s⁰) -> (Expattern s² s¹ s⁰) -> Maybe (CAS s² s¹ s⁰)
fillGaps matches (Gap i)
  | rematch@(Just _) <- Map.lookup i matches  = rematch
fillGaps matches (Symbol s) = Just $ Symbol s
fillGaps matches (Function f x) = Function f <$> fillGaps matches x
fillGaps matches (OperatorChain x ys)
    = OperatorChain <$> fillGaps matches x
                    <*> sequence [ (o,) <$> fillGaps matches y | (o,y) <- ys ]
fillGaps _ _ = Nothing

exploreEquality :: (Eq s⁰, Eq s¹, Eq s²)
           => [Expattern s² s¹ s⁰] -> CAS s² s¹ s⁰ -> Equality s² s¹ s⁰
exploreEquality tfms = undefined
