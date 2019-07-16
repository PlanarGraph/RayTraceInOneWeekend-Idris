module Hitable

import Control.Monad.State
import Data.Vect
import public Vect3
import public Ray

%access public export

record HitRecord where
  constructor MkHR
  t : Double
  p : Vect3
  normal : Vect3

data Hitable =
    Sphere Vect3 Double

discriminant : Hitable -> Ray -> (Double, Double, Double)
discriminant (Sphere center radius) (MkRay origin direction) 
  = let oc = origin - center
        a = dot direction direction
        b = dot oc direction
        c = (dot oc oc) - (radius * radius) in
        (a,b,c)

hit : Hitable -> Ray -> Double -> Double -> Maybe HitRecord
hit s@(Sphere center radius) r@(MkRay origin direction) tMin tMax = 
  let (a, b, c) = discriminant s r 
      temp1 = ((-b) - sqrt (b * b - a * c)) / a
      temp2 = ((-b) + sqrt (b * b - a * c)) / a in
      if (b * b - a * c) > 0 
         then case (temp1 < tMax && temp1 > tMin, temp2 < tMax && temp2 > tMin) of
                   (True, _) => Just $ updateRec temp1
                   (_, True) => Just $ updateRec temp2
                   (_, _) => Nothing
         else Nothing
  where
    updateRec : Double -> HitRecord
    updateRec up = let np = point_at_parameter r up
                       nnormal = map (/radius) (np - center) in
                       (MkHR up np nnormal)
