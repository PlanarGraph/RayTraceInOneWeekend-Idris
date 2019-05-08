module Vect3

import Data.Vect

%access public export

Vect3 : Type
Vect3 = Vect 3 Double

getX : Vect3 -> Double
getX (x :: _) = x

getY : Vect3 -> Double
getY (x :: y :: _) = y

getZ : Vect3 -> Double
getZ (x :: y :: z :: _) = z

dot : Vect3 -> Vect3 -> Double
dot x y = sum $ zipWith (*) x y

cross : Vect3 -> Vect3 -> Vect3
cross (x1 :: y1 :: z1 :: []) (x2 :: y2 :: z2 :: [])
  = let v1 = y1 * z2 - z1 * y2
        v2 = (-1) * x1 * z2 - z1 * x2
        v3 = x1 * y2 - y1 * x2 in
        [v1,v2,v3]

sqrLen : Vect3 -> Double
sqrLen x = foldr (\a, b => a * a + b) 0 x

len : Vect3 -> Double
len x = sqrt $ sqrLen x

unit : Vect3 -> Vect3
unit x = map (/(len x)) x

makeUnitVect : Vect3 -> Vect3
makeUnitVect x = let k = 1.0 / (len x) in
                     map (*k) x

Num Vect3 where
  (+) x y = zipWith (+) x y
  (*) x y = zipWith (*) x y
  fromInteger x = [cast x,0,0]

public export
Neg Vect3 where
  negate x = map (*(-1)) x
  (-) x y = zipWith (-) x y

Fractional Vect3 where
  (/) x y = zipWith (/) x y
  recip x = map (1/) x
