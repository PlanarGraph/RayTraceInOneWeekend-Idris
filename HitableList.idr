module HitableList

import Data.Vect
import Control.Monad.State
import public Hitable
import public Ray
import public Sphere

%access public export 

data HitableList : Type -> Type where
  MkHL : Hitable a => List a -> HitableList a

hitL : HitableList a -> Ray -> Double -> Double -> Maybe HitRecord
hitL (MkHL list) r tMin tMax =
  case foldl findClosest (Nothing ,tMax) list of
       (a,b) => a
  where
    findClosest : Hitable a => (Maybe HitRecord, Double) -> a -> (Maybe HitRecord, Double)
    findClosest b@(hr, closest) hitable =
          let x = hit hitable r tMin closest in
              case x of
                   Nothing => b
                   Just nhr => (Just nhr, t nhr)
