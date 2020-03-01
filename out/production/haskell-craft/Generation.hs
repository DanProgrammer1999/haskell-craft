module Generation where

import           Constants
import           World
import           System.Random

-- Generate random point in a chunk
randomChunkPoint :: StdGen -> (RelativePosition, StdGen)
randomChunkPoint g = ((randX, randY, randZ), finalG)
  where
    (randX, g') = randomR (0, fromIntegral chunkSize) g
    (randY, g'') = randomR (0, fromIntegral maxYLevel) g'
    (randZ, finalG) = randomR (0, fromIntegral chunkSize) g''