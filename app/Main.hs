module Main where

import Generation
import World
import NoiseGen.Noise
import System.Random
import CodeWorld

main :: IO ()
main = do
  let chunk = (1, 1)
  g <- getStdGen
  let (blockPosition, newG) = randomChunkPoint g
  
  putStrLn ("Random point: " ++ (show blockPosition))
  
  let absPosition = toAbsolutePosition chunk blockPosition
  putStrLn ("Absolute position: " ++ show absPosition)
  
  let chunk = generateChunk 10 Normal Plains
  putStrLn (show chunk)