module randomEx

import Effects
import Effect.Random
import Effect.StdIO
import Effect.System

fn : Eff () [RND, STDIO, SYSTEM]
fn = do
  v <- rndInt 0 100
  y <- rndInt 0 100
  putStrLn $ show v
  putStrLn $ show y

main : IO ()
main = do
  t <- run time
  run (srand t)
  v <- [1..5] >>= fn
  putStrLn "Done"
