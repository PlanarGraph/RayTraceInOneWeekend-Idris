module Camera

import Data.Vect
import public Vect3
import public Ray

%access public export

record Camera where
  constructor MkCamera
  lowerLeftCorner : Vect3
  horizontal : Vect3
  vertical : Vect3
  origin : Vect3

defaultCamera : Camera
defaultCamera = 
  MkCamera [-2.0, -1.0, -1.0] [4.0, 0.0, 0.0] [0.0, 2.0, 0.0] [0.0, 0.0, 0.0]

getRay : Camera -> Double -> Double -> Ray
getRay (MkCamera lowerLeftCorner horizontal vertical origin) u v =
  MkRay origin ((lowerLeftCorner + map (*u) horizontal + map (*v) vertical) - origin)
