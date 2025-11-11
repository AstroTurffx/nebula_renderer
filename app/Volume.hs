module Volume where

import Types
import NoiseGenerator
import Transfer

import qualified Numeric.Noise as Noise
import Debug.Trace (trace)

-- dz = Ray step distance, total distance / num steps
dz, scale, cutoff, reflectivity :: Float
dz = 2.0 / 128.0
scale = 2.5
cutoff = 0.5
reflectivity = 0.5

colorSampleSteps :: Int
colorSampleSteps = 8

sampleVolume :: Volume -> Vec3f -> Float
sampleVolume Sphere (x,y,z) = exp (- ((r * 3) ^ 2))
    where r = sqrt (x*x + y*y + z*z)

sampleVolume (Cloud generator seed) (x,y,z)
    | n <= cutoff = 0.0
    | otherwise   = normalRange (cutoff, 1) n
    where
        n = generator seed $ scale3 scale (x,y,z)

-- TODO phase function, light intensity falloff and composite nebulas
orthoRayVolume :: Volume -> TransferFunc -> [LightSource] -> Vec2f -> Vec3f
orthoRayVolume volume tf lightSources (x, y) = go (-1) (0, 0, 0) 0.0
    where
        -- transmisisonMult = 5
        go z (rAcc, gAcc, bAcc) tAcc
            | z > 1 || tAcc >= 0.99 = (rAcc, gAcc, bAcc)
            | otherwise =
                let d = sampleVolume volume (x, y, z)
                    ((r,g,b), (er,eg,eb), opacity) = tf d
                    alpha = opacity * dz

                    -- How much of this color will add to the accumulated color
                    transmission = (1 - tAcc) * alpha
                    viewRay = (0,0,1)
                    (sr, sg, sb) = foldr1 add3 [ incomingLightAt l volume tf (x,y,z) viewRay | l <- lightSources ]

                    -- add emitted + scattered light
                    rAcc' = rAcc + transmission * (r * sr + er)
                    gAcc' = gAcc + transmission * (g * sg + eg)
                    bAcc' = bAcc + transmission * (b * sb + eb)
                    tAcc' = tAcc + transmission
                in go (z + dz) (rAcc', gAcc', bAcc') tAcc'

--------------------------
-------- Lighting --------
--------------------------
lightDirection :: LightSource -> Vec3f -> Vec3f
lightDirection (Point, lightPos, _)  point = normalise3 $ lightPos `sub3` point
lightDirection (Directional, dir, _) _     = normalise3 . scale3 (-1) $ dir

lightSampleStep :: LightSource -> Vec3f -> Vec3f
lightSampleStep (Point, lightPos, _)  point = scale3 (1.0 / fromIntegral (colorSampleSteps+1))    . normalise3 $ lightPos `sub3` point
lightSampleStep (Directional, dir, _) _     = scale3 ((-2.0) / fromIntegral (colorSampleSteps+1)) . normalise3 . scale3 (-1) $ dir

incomingLightAt :: LightSource -> Volume -> TransferFunc -> Vec3f -> Vec3f -> Vec3f
incomingLightAt source volume tf startPos viewRay = scale3 phase $ sample 0 (add3 startPos step) (0,0,0) 0
    where
        transmisisonMult = 2 -- Its too easy for the light to reach the point
        (_, _, lightColor) = source

        step = lightSampleStep source startPos
        stepSize = magnitude3 step 

        phase = hgPhase reflectivity $ dot viewRay $ lightDirection source startPos 
        sample i pos cAcc tAcc
            | tAcc >= 0.99 = cAcc
            | i >= colorSampleSteps =
                let transmission = 1 - tAcc
                in add3 cAcc $ scale3 transmission lightColor
            | otherwise =
                let d = sampleVolume volume pos
                    (c, e, opacity) = tf d
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