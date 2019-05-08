module Ray

import Data.Vect
import public Vect3

%access public export

record Ray where
  constructor MkRay
  origin : Vect3
  direction : Vect3

point_at_parameter : Ray -> Double -> Vect3
point_at_parameter (MkRay origin direction) y = origin + direction * [y,y,y]
