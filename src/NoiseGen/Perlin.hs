-- Based on https://github.com/colinhect/hsnoise Colin Hill, Copyright (c) 2011

-- | Implementation of Perlin noise.
--
-- Example of use:
--
-- @
--main = putStrLn (\"Noise value at (1, 2, 3): \" ++ show x)
--    where seed        = 1
--          octaves     = 5
--          scale       = 0.05
--          persistence = 0.5
--          perlinNoise = Perlin seed octaves scale persistence
--          x           = noiseValue perlinNoise (1, 2, 3)
-- @

module NoiseGen.Perlin (
    Perlin,
    noiseValue
) where

import NoiseGen.Noise

-- | A Perlin noise function.
data Perlin = Perlin
  {
    seed        :: Seed,
    octaves     :: Int,
    scale       :: Double,
    persistence :: Double
  }

-- | Produce a value of Perlin noise at the given point
noiseValue :: Perlin -> Point -> Double
noiseValue perlinNoise point = clamp noise (-1) 1
  where
    noise = perlinNoiseValue perlinNoise (octaves perlinNoise) 1 1 point

    perlinNoiseValue _ 0 _ _ _ = 0
    perlinNoiseValue perlinNoise oct freq amp p =
      noise + perlinNoiseValue perlinNoise oct' freq' amp' point
      where
        -- Octave count
        oct'   = oct - 1
        freq'  = freq * 2
        amp'  = amp * persistence perlinNoise
        point' = pmap (* (scale perlinNoise * freq)) point
        noise  = coherentNoise (seed perlinNoise) point' * amp