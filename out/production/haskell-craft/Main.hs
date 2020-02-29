module Main where

import Generation
import System.Random

main :: IO ()
main = do
  let chunk = (1, 1)
  g <- getStdGen
  let (block, newG) = randomChunkPoint g
  
  putStrLn ("Random point: " ++ (show block))
  
  let absPosition = toAbsolutePosition chunk block
  putStrLn ("Absolute position: " ++ show absPosition)