module Main

import Data.Vect
import public Vect3
import public Ray
import public Sphere
import public Hitable
import public HitableList
import public Camera
import Effects
import Effect.StdIO
import Effect.Random
import Effect.System

genRand : Int -> Int -> Eff Double [RND]
genRand u nu = do v <- rndInt 0 9999999999999998
                  let fv = the Double $ (cast v) / 10000000000000000.0
                  pure $ the Double $ (cast u + fv) / cast nu

color : Hitable a => Ray -> HitableList a -> Vect3 
color r@(MkRay origin direction) world =
  case hitL world r 0.0 9.9e999 of
       Nothing => let unitDir = unit direction
                      t = 0.5 * (getY unitDir) + 1.0 in
                      map (*(1.0-t)) [1.0, 1.0, 1.0] + map (*t) [0.5, 0.7, 1.0]
       Just (MkHR t p normal) => map (\x => 0.5 * (x+1)) normal


printVect3 : Vect 3 Int -> String
printVect3 (x :: y :: z :: []) = show x ++ " " ++ show y ++ " " ++ show z

iter2 : Hitable a => Int -> Int -> Int -> Int -> Camera -> HitableList a ->
        Vect3 -> Int -> Eff Vect3 [STDIO, RND]
iter2 i j nx ny camera world col 0 = pure col
iter2 i j nx ny camera world col n = do
  u <- genRand i nx
  v <- genRand j ny
  let r = getRay camera u v
  iter2 i j nx ny camera world (col + color r world) (n-1)

helper2 : Hitable a => Int -> Int -> Int -> Camera -> HitableList a ->
          (Int, Int) -> Eff () [RND, STDIO, SYSTEM]
helper2 nx ny ns camera world (j, i) = do
    t <- time
    srand t
    col <- iter2 i j nx ny camera world [0,0,0] ns
    putStrLn $ printVect3 $ map (the Int . cast) $ map outMap col
  where
    outMap : Double -> Double
    outMap x = (*) 255.99 $ x / (cast ns)

main : IO ()
main =
  let nx = the Int 200
      ny = the Int 100
      ns = the Int 100
      header = the (List String) ["P3", show nx, show ny, "255"]
      camera = defaultCamera
      world = MkHL [MkSphere [0,0,-1] 0.5, MkSphere [0,-100.5,-1] 100]
      hlp = (helper2 nx ny ns camera world) in do
      traverse_ putStrLn header
      for_ [ (j, i) | j <- [(ny-1),(ny-2)..0], i <- [0..(nx-1)]] $ \v => do
        run $ hlp v
