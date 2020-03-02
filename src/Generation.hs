module Generation where

import           Constants
import           World
import           NoiseGen.Perlin
import           NoiseGen.Noise (Seed)
import           Data.Int
import           System.Random

-- Generate random point in a chunk
randomChunkPoint :: StdGen -> (RelativePosition, StdGen)
randomChunkPoint g = ((randX, randY, randZ), finalG)
  where
    (randX, g') = randomR (0, fromIntegral chunkSize) g
    (randY, g'') = randomR (0, fromIntegral maxYLevel) g'
    (randZ, finalG) = randomR (0, fromIntegral chunkSize) g''

-- | All points (X, Z) of a chunk
allChunkLevelPoints :: [(Int8, Int8)]
allChunkLevelPoints 
  = (,) <$> [0 .. (fromIntegral chunkSize - 1)] <*> [0 .. (fromIntegral chunkSize - 1)]

generateChunk :: Seed -> WorldType -> Biome -> [Block]
generateChunk _ Flat _          = flatBedrock ++ terrain
  where
    terrain = (\(x, z) y -> Block (x, z, y) Grass) <$> allChunkLevelPoints <*> [1, 2]
        
generateChunk seed Normal Plains = terrain
  where
    octaves = 4
    scale = 0.3
    persistence = 0.5
    perlinDist = Perlin seed octaves scale persistence
    threshold = 0.5
    
    calculateHeight x z = toHeight $ noiseValue perlinDist (x, z, fromIntegral seaLevel)
    toHeight noiseVal   = fromIntegral $ floor (noiseVal * 10) + seaLevel
    
    blocks = map (\(x, z) -> (x, z, calculateHeight (fromIntegral x) (fromIntegral z))) allChunkLevelPoints
    
    terrain = map (flip Block Stone) blocks


flatBedrock :: [Block]
flatBedrock = map (\(x, z) -> Block (x, z, 0) Bedrock) allChunkLevelPoints
