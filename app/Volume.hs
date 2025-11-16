module Volume where

import Types
import NoiseGenerator
import Transfer

import qualified Numeric.Noise as Noise
import Debug.Trace (trace)

-- dz = Ray step distance, total distance / num steps
dz, scale, cutoff, reflectivity :: Float
dz = 2.0 / 16.0
scale = 1.3
cutoff = 0.52
reflectivity = 0.6

warpFreq, warpStrength :: Float
warpFreq = 0.07
warpStrength = 25

lightSampleSteps :: Int
lightSampleSteps = 4

sampleVolume :: Volume -> Vec3f -> [Float]
sampleVolume Sphere (x,y,z) = [exp (- ((r * 3) ^ 2))]
    where r = sqrt (x*x + y*y + z*z)

sampleVolume (Cloud falloff generator seed) (x,y,z)
    | n <= cutoff = [0.0]
    | otherwise   = [normalRange (cutoff, 1) n]
    where n = fallOffValue falloff (x,y,z) * generator seed (scale3 scale (x,y,z))

sampleVolume (CompositeCloud falloff generator seed numElements) pos = foldr1 (++) [
    let innerNoise = generator (seed+i)
        warpPos = add3 pos $ warpSample3 warpStrength warpFreq innerNoise pos
        density = fallOffValue falloff pos * generator seed (scale3 scale warpPos)
    in [clamp 0 1 . normalRange (cutoff, 1) $ density]
    | i <- [1..numElements]]

orthoRayVolume :: Volume -> [TransferFunc] -> [LightSource] -> Vec2f -> Vec4f
orthoRayVolume volume tfs lightSources (x, y) = go (-1) (0, 0, 0, 0)
    where
        -- transmisisonMult = 2
        go z (rAcc, gAcc, bAcc, tAcc)
            | z > 1 || tAcc >= 0.99 = (rAcc, gAcc, bAcc, tAcc)
            | otherwise =
                let ds = sampleVolume volume (x, y, z)
                    ((r,g,b), (er,eg,eb), opacity) = multiTransfer $ zip tfs ds
                    alpha = opacity * dz

                    -- How much of this color will add to the accumulated color
                    transmission = (1 - tAcc) * alpha
                    viewRay = (0,0,1) -- orthgraphic view therefore constant ray
                    (sr, sg, sb) = foldr1 add3 [ incomingLightAt l volume tfs (x,y,z) viewRay | l <- lightSources ]

                    -- add emitted + scattered light
                    rAcc' = rAcc + transmission * (r * sr + er)
                    gAcc' = gAcc + transmission * (g * sg + eg)
                    bAcc' = bAcc + transmission * (b * sb + eb)
                    tAcc' = tAcc + transmission
                in go (z + dz) (rAcc', gAcc', bAcc', tAcc')

--------------------------
-------- Lighting --------
--------------------------
lightDirection :: LightSource -> Vec3f -> Vec3f
lightDirection (Point, lightPos, _)  point = normalise3 $ lightPos `sub3` point
lightDirection (Directional, dir, _) _     = normalise3 . scale3 (-1) $ dir

lightSampleStep :: LightSource -> Vec3f -> Vec3f
lightSampleStep (Point, lightPos, _)  point = scale3 (1.0 / fromIntegral (lightSampleSteps+1))    . normalise3 $ lightPos `sub3` point
lightSampleStep (Directional, dir, _) _     = scale3 ((-2.0) / fromIntegral (lightSampleSteps+1)) . normalise3 . scale3 (-1) $ dir

incomingLightAt :: LightSource -> Volume -> [TransferFunc] -> Vec3f -> Vec3f -> Vec3f
incomingLightAt source volume tfs startPos viewRay = scale3 phase $ sample 0 (add3 startPos step) (0,0,0) 0
    where
        transmisisonMult = 0.85 -- Its too easy for the light to reach the point
        (_, _, lightColor) = source

        step = lightSampleStep source startPos
        stepSize = magnitude3 step

        phase = hgPhase reflectivity $ dot viewRay $ lightDirection source startPos
        sample i pos cAcc tAcc
            | tAcc >= 0.99 = cAcc
            | i >= lightSampleSteps =
                let transmission = 1 - tAcc
                in add3 cAcc $ scale3 transmission lightColor
            | otherwise =
                let ds = sampleVolume volume pos
                    (c, e, opacity) = multiTransfer $ zip tfs ds
                    transmission = (1 - tAcc) * (opacity * stepSize) * transmisisonMult
                    cAcc' = add3 cAcc $ scale3 transmission $ add3 c e
                    tAcc' = tAcc + transmission
                in sample (i+1) (add3 pos step) cAcc' tAcc'

-- Henyey-Greenstien Phase Function
hgPhase :: Float -> Float -> Float
hgPhase g cosTheta =
    let g2 = g * g
        denom = (1 + g2 - 2*g*cosTheta) ** 1.5
    in (1 - g2) / (4*pi * denom)