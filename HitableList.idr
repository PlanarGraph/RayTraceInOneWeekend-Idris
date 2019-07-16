module HitableList

import Data.Vect
import Control.Monad.State
import public Hitable
import public Ray

%access public export 

HitableList : Type
HitableList = List Hitable

hitL : HitableList -> Ray -> Double -> Double -> Maybe HitRecord
hitL list r tMin tMax =
  case foldl findClosest (Nothing ,tMax) list of
       (a,b) => a
  where
    findClosest : (Maybe HitRecord, Double) -> Hitable -> (Maybe HitRecord, Double)
    findClosest b@(hr, closest) hitable =
          let x = hit hitable r tMin closest in
              case x of
                   Nothing => b
                   Just nhr => (Just nhr, t nhr)
