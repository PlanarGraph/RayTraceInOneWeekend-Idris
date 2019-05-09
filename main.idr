module Main

import Data.Vect
import public Vect3
import public Ray
import public Sphere
import public Hitable
import public HitableList

color : Hitable a => Ray -> HitableList a -> Vect3 
color r@(MkRay origin direction) world =
  case hitL world r 0.0 9.9e999 of
       Nothing => let unitDir = unit direction
                      t = 0.5 * (getY unitDir) + 1.0 in
                      map (*(1.0-t)) [1.0, 1.0, 1.0] + map (*t) [0.5, 0.7, 1.0]
       Just (MkHR t p normal) => map (\x => 0.5 * (x+1)) normal


printVect3 : Vect 3 Int -> String
printVect3 (x :: y :: z :: []) = show x ++ " " ++ show y ++ " " ++ show z

main : IO ()
main =  
  let nx = the Int 200
      ny = the Int 100
      header = the (List String) ["P3", show nx, show ny, "255"]
      lower_left_corner = [-2.0, -1.0, -1.0]
      horizontal = [4.0, 0.0, 0.0]
      vertical = [0.0, 2.0, 0.0]
      origin = [0.0, 0.0, 0.0]
      list = MkHL [MkSphere [0,0,-1] 0.5, MkSphere [0,-100.5,-1] 100]
      hlp = helper nx ny lower_left_corner horizontal vertical origin list in do
  traverse_ putStrLn header
  traverse_ putStrLn [ hlp j i | j <- [(ny-1),(ny-2)..0], i <- [0..(nx-1)] ]
where
  helper : Hitable a => Int -> Int -> Vect3 -> Vect3 -> Vect3 -> Vect3 -> HitableList a -> Int -> Int -> String
  helper nx ny lower_left_corner horizontal vertical origin lst j i
    = let u = the Double $ cast i / cast nx
          v = the Double $ cast j / cast ny
          r = MkRay origin (lower_left_corner + map (*u) horizontal + map (*v) vertical)
          col = color r lst
          vout = map (*255.99) col in
          printVect3 $ map (the Int . cast) vout
