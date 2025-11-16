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
-- DEPRECATED
applyFallOff :: FallOff -> Float -> NoiseGenerator -> NoiseGenerator
applyFallOff (Distance interpType start end) scale ngen seed pos
    | r <= start = value
    | r >= end   = 0.0
    | otherwise =
        let
            t = normalRange (start, end) r
            s = interp interpType t -- interp func
        in (1.0 - s) * value
    where
        (x,y,z) = scale3 (1/scale) pos
        r = sqrt (x*x + y*y + z*z)
        value = ngen seed pos
applyFallOff (Cube interpType start end) scale ngen seed pos = (1.0 - r) * value
    where
        (x,y,z) = scale3 (1/scale) pos
        f a | a < start = 0.0
            | a > end   = 1.0
            | otherwise = interp interpType . normalRange (start,end) $ a
        g = f . abs
        r = clamp 0 1 $ g x + g y + g z
        value = ngen seed pos

-- TAKES IN SCREEN POSITION NOT WARPED POSITION
fallOffValue :: FallOff -> Vec3f -> Float
fallOffValue (Distance interpType start end) (x,y,z)
    | r <= start = 1.0
    | r >= end   = 0.0
    | otherwise = 1 - (interp interpType . normalRange (start, end)) r
    where r = sqrt (x*x + y*y + z*z)
fallOffValue (Cube interpType start end) (x,y,z)
    | r <= start = 1.0
    | r >= end   = 0.0
    | otherwise = 1 - (interp interpType . normalRange (start, end)) r
    where r = maximum [abs x, abs y, abs z] -- chebyshev distance
    

-- !! For warping not normal sampling
warpSample3 :: Float -> Float -> NoiseSampler -> Vec3f -> Vec3f
warpSample3 strength freq sampler pos = 
    let (x,y,z) = scale3 freq pos
        out = ( sampler (x + 2.0, y + 0.1, z + 7.9)
            , sampler (x + 5.3, y + 1.3, z + 0.5)
            , sampler (x + 1.7, y + 9.2, z + 2.8)
            ) -- just adding arbitary values
    in scale3 strength out

-- Disgustingly slow
warp3 :: Noise.Noise3 Float -> NoiseGenerator
warp3 noise seed (x,y,z) = (1 + Noise.noise3At outerNoise (fromIntegral seed) x y z) / 2
    where
        warpFreq = 0.07
        warpStrength = 20.0
        innerNoise :: NoiseSampler
        innerNoise (ix,iy,iz) = Noise.noise3At (Noise.next3 noise) (fromIntegral seed) ix iy iz
        outerNoise = Noise.warp (\v -> (add3 v . warpSample3 warpStrength warpFreq innerNoise) v) $ Noise.next3 noise



-- worley
-- generationRange :: Vec3f
-- generationRange = (0.8, 0.8, 0.8)

-- worley :: Int -> NoiseGenerator
-- worley octaves seed point = sum [ worleyAtOctave * 0.5^n | n <- [1..(octaves+1)] ]


-- worleyAtOctave :: Int -> NoiseGenerator
-- worleyAtOctave