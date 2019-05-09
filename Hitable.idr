module Hitable

import Control.Monad.State
import public Vect3
import public Ray

%access public export

record HitRecord where
  constructor MkHR
  t : Double
  p : Vect3
  normal : Vect3

interface Hitable a where
  hit : a -> Ray -> Double -> Double -> Maybe HitRecord
