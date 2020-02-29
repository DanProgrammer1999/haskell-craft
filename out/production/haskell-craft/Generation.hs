module Generation where

import           Constants
import           System.Random

-- TODO consider moving type declarations to more general scope
type ChunkIndex = (Integer, Integer)

type AbsolutePosition = (Double, Double, Double)

type BlockPosition = (Int, Int, Int)

-- | Given starting position, current chunk, and the position in the current chunk,
-- calculate the absolute position (bottom corner)
toAbsolutePosition
  :: ChunkIndex       -- ^ Chunk number
  -> BlockPosition    -- ^ Block index in the chunk
  -> AbsolutePosition -- ^ Absolute position in the world (center of the block)
toAbsolutePosition (chunkX, chunkZ) (blockX, blockY, blockZ) = (xOffset, yOffset, zOffset)
  where
    xOffset = fromIntegral (blockX + (fromIntegral chunkX) * chunkSize) * blockSize
    zOffset = fromIntegral (blockZ + (fromIntegral chunkZ) * chunkSize) * blockSize
    yOffset = (fromIntegral blockY) * blockSize

-- Generate random point in a chunk
randomChunkPoint :: StdGen -> (BlockPosition, StdGen)
randomChunkPoint g = ((randX, randY, randZ), finalG)
  where
    (randX, g') = randomR (0, chunkSize) g
    (randY, g'') = randomR (0, maxYLevel) g'
    (randZ, finalG) = randomR (0, chunkSize) g''

