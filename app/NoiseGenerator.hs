module NoiseGenerator where

import Types
import Transfer

import qualified Numeric.Noise as Noise

-- type ScalarField3D = [[[Float]]] -- temp
-- data SF3DGenerator = Perlin | Worley

-- seed -> point -> value [0,1]
toNoiseGenerator :: Noise.Noise3 Float -> NoiseGenerator
toNoiseGenerator noise = foo
    where
        foo :: NoiseGenerator
        foo seed (x,y,z) = (1 + Noise.noise3At noise (fromIntegral seed) x y z) / 2

-- Would make more sense to apply to a NoiseSampler but i don't rly use it
applyFallOff :: FallOff -> NoiseGenerator -> NoiseGenerator
applyFallOff (Distance interpType start end) ngen seed (x,y,z)
    | r <= start = value
    | r >= end   = 0.0
    | otherwise =
        let
            t = normalRange (start, end) r
            s = interp interpType t -- interp func
        in (1.0 - s) * value
    where
        r = sqrt (x*x + y*y + z*z)
        value = ngen seed (x,y,z)

sample3 :: NoiseSampler -> Vec3f -> Vec3f
sample3 sampler (x,y,z) = 
    ( sampler (x + 2.0, y + 0.1, z + 7.9)
    , sampler (x + 5.3, y + 1.3, z + 0.5)
    , sampler (x + 1.7, y + 9.2, z + 2.8)
    ) -- just adding arbitary values

-- Disgustingly slow
warp3 :: Noise.Noise3 Float -> NoiseGenerator
warp3 noise seed (x,y,z) = (1 + Noise.noise3At outerNoise (fromIntegral seed) x y z) / 2
    where
        warpFreq = 0.05
        warpStrength = 25.0
        innerNoise :: NoiseSampler
        innerNoise (ix,iy,iz) = Noise.noise3At (Noise.warp (scale3 warpFreq) $ Noise.next3 noise) (fromIntegral seed) ix iy iz
        outerNoise = Noise.warp (\v -> (add3 v . scale3 warpStrength . sample3 innerNoise) v) $ Noise.next3 noise



-- worley
-- generationRange :: Vec3f
-- generationRange = (0.8, 0.8, 0.8)

-- worley :: Int -> NoiseGenerator
-- worley octaves seed point = sum [ worleyAtOctave * 0.5^n | n <- [1..(octaves+1)] ]


-- worleyAtOctave :: Int -> NoiseGenerator
-- worleyAtOctave