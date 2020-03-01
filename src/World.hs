module World where

import           Data.Int
import           Constants

data World = World
    {
      spawnBlock     :: RelativePosition,
      chunks         :: [Chunk],
      playerPosition :: AbsolutePosition
    }

data Biome 
  = Flat 
  | Plains 
  deriving (Eq, Show)

data Chunk = Chunk
    {
      chunkIndex :: ChunkIndex,
      terrain    :: [Block],
      biome      :: Biome
    }

data BlockType
  = Grass
  | Stone
  | Bedrock
  deriving (Show, Eq)

data Block = Block
    {
      position  :: RelativePosition, -- ^ x, y, and z coordinates of the block relative to the chunk
      blockType :: BlockType         -- ^ Type of the block
    }
    deriving (Show)

type ChunkIndex       = (Integer, Integer)
type RelativePosition = (Int8, Int16, Int8)
type AbsolutePosition = (Integer, Integer, Integer)

-- | Given starting position, current chunk, and the position in the current chunk,
-- calculate the absolute position (bottom corner)
toAbsolutePosition :: ChunkIndex -> RelativePosition -> AbsolutePosition
toAbsolutePosition (xIndex, zIndex) (xBlock, yBlock, zBlock) = (xOffset, yOffset, zOffset)
  where
    xOffset = fromIntegral xBlock + xIndex * chunkSize
    zOffset = fromIntegral zBlock + zIndex * chunkSize
    yOffset = fromIntegral yBlock

toRelativePosition :: AbsolutePosition -> (ChunkIndex, RelativePosition)
toRelativePosition (absX, absY, absZ) = ((chunkX, chunkZ), (fromIntegral blockX, blockY, fromIntegral blockZ))
  where
    (chunkX, blockX) = absX `divMod` chunkSize
    (chunkZ, blockZ) = absZ `divMod` chunkSize
    blockY = fromIntegral absY