module CAS.Dumb.Tree where

import CAS.Dumb.Util.These

data CAS g = Symbol String
type Ept = CAS Int

mp :: Ept -> CAS () -> Maybe (Int, CAS ())
mp _ _ = tuc


