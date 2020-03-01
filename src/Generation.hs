module Generation where

import           Constants
import           System.Random
import           World

-- Generate random point in a chunk
randomChunkPoint :: StdGen -> (RelativePosition, StdGen)
randomChunkPoint g = ((randX, randY, randZ), finalG)
  where
    (randX, g') = randomR (0, fromIntegral chunkSize) g
    (randY, g'') = randomR (0, fromIntegral maxYLevel) g'
    (randZ, finalG) = randomR (0, fromIntegral chunkSize) g''

generateChunk :: Biome -> g -> [Block]
generateChunk Flat _ = flatBedrock ++ terrain
  where
    terrain = 
      zipWith3
        (\x y z -> Block (x, y, z) Grass)
        [0 .. (fromIntegral chunkSize - 1)]
        [0, 1]
        [0 .. (fromIntegral chunkSize - 1)]
generateChunk Plains g = flatBedrock

flatBedrock :: [Block]
flatBedrock =
  zipWith (\x z -> Block (x, 0, z) Bedrock) [0 .. (fromIntegral chunkSize - 1)] [0 .. (fromIntegral chunkSize - 1)]
