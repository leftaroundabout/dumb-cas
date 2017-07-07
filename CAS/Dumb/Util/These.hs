module CAS.Dumb.Util.These where

import Data.Map (Map)

traverseUnionConflicts :: (Applicative t, Ord k)
            => (a -> a -> t a) -> Map k a -> Map k a -> t (Map k a)
traverseUnionConflicts = undefined
