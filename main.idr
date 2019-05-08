module Main

import Data.Vect
import public Vect3
import public Ray

hitSphere : Vect3 -> Double -> Ray -> Bool
hitSphere v radius (MkRay origin direction) = let oc = origin - v
                                                  a = direction `dot` direction
                                                  b = 2.0 * (dot oc direction)
                                                  c = (dot oc oc) - (radius * radius)
                                                  discriminant = (b * b) - (4.0 * a * c) in
                                                  discriminant > 0

color : Ray -> Vect3 
color r@(MkRay origin direction) = let unit_direction = unit direction
                                       t = 0.5 * (getY unit_direction + 1) in
                                       if hitSphere [0,0,-1] 0.5 r
                                          then [1,0,0]
                                          else map (*(1.0-t)) [1.0, 1.0, 1.0] + map (*t) [0.5, 0.7, 1.0]

printVect3 : Vect 3 Int -> String
printVect3 (x :: y :: z :: []) = show x ++ " " ++ show y ++ " " ++ show z

main : IO ()
main = do 
  let nx = the Int 200
  let ny = the Int 100
  let header = the (List String) ["P3", show nx, show ny, "255"]
  let lower_left_corner = [-2.0, -1.0, -1.0]
  let horizontal = [4.0, 0.0, 0.0]
  let vertical = [0.0, 2.0, 0.0]
  let origin = [0.0, 0.0, 0.0]
  traverse_ putStrLn header
  for_ [ helper j i nx ny lower_left_corner horizontal vertical origin | j <- [(ny-1),(ny-2)..0], i <- [0..(nx-1)] ] putStrLn
where
  helper : Int -> Int -> Int -> Int -> Vect3 -> Vect3 -> Vect3 -> Vect3 -> String
  helper j i nx ny lower_left_corner horizontal vertical origin
    = let u = the Double $ cast i / cast nx
          v = the Double $ cast j / cast ny
          r = MkRay origin (lower_left_corner + map (*u) horizontal + map (*v) vertical)
          col = color r
          vout = map (*255.99) col in
          printVect3 $ map (the Int . cast) vout
