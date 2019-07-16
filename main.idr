module Main

import Data.Vect
import public Vect3
import public Ray
import public Hitable
import public HitableList
import public Camera
import Effects
import Effect.StdIO
import Effect.Random
import Effect.System

genRand : Int -> Int -> Eff Double [RND]
genRand u nu = do v <- (the Double . cast) <$> rndInt 0 99999998
                  pure $ the Double $ (cast u + v / 100000000) / cast nu

color : Ray -> HitableList -> Vect3 
color r@(MkRay origin direction) world =
  case hitL world r 0.0 9.9e999 of
       Nothing => let unitDir = unit direction
                      t = 0.5 * (getY unitDir) + 1.0 in
                      map (*(1.0-t)) [1.0, 1.0, 1.0] + map (*t) [0.5, 0.7, 1.0]
       Just (MkHR t p normal) => map (\x => 0.5 * (x+1)) normal

printVect3 : Vect 3 Int -> String
printVect3 (x :: y :: z :: []) = show x ++ " " ++ show y ++ " " ++ show z

getSample : Int -> Int -> Int -> Int -> Camera -> HitableList -> Eff Vect3 [RND]
getSample i j nx ny camera world = do
  u <- genRand i nx
  v <- genRand j ny
  let r = getRay camera u v
  pure $ color r world

startT : Int -> Int -> Int -> Camera -> HitableList -> Eff (List String) [SYSTEM, RND]
startT nx ny ns camera world = do
    seed <- time
    srand seed
    strs <- (flip mapE) [(j, i) | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]] $ \(j, i) => do
      vals <- mapE (\_ => getSample i j nx ny camera world) [1..ns]
      pure $ printVect3 $ map (the Int . cast) $ map (\x => (*) 255.99 $ x / (cast ns)) $ sum vals
    pure strs
  where
    outMap : Double -> Double
    outMap x = (*) 255.99 $ x / cast ns

main : IO ()
main =
  let nx = the Int 200
      ny = the Int 100
      ns = the Int 100
      header = the (List String) ["P3", show nx, show ny, "255"]
      camera = defaultCamera
      world = [Sphere [0,0,-1] 0.5, Sphere [0,-100.5,-1] 100] in do
      traverse_ putStrLn header
      image <- run $ startT nx ny ns camera world
      traverse_ (\x => putStrLn x) image
