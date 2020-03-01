module Main where

import Generation
import World
import System.Random

main :: IO ()
main = do
  let chunk = (1, 1)
  g <- getStdGen
  let (blockPosition, newG) = randomChunkPoint g
  
  putStrLn ("Random point: " ++ (show blockPosition))
  
  let absPosition = toAbsolutePosition chunk blockPositi
  putStrLn ("Absolute position: " ++ show absPosition)