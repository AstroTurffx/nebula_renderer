module Transfer where

import Types
import Debug.Trace (trace)

-----------------------------------------
-------- Interpolation Functions --------
-----------------------------------------

-- Interpolate from [0,1] to [0,1]
-- WARNING: doesn't clamp to [0,1], therefore can produce weird results outside 0-1
interp :: InterpolationType -> Float -> Float
interp Linear       x = x
interp SmoothStep   x = x*x*(3 - 2*x)
interp EaseInCubic  x = x^3
interp EaseOutCubic x = 1 - (1-x)^3

interpBetween :: InterpolationType -> Float -> Float -> Float -> Float
interpBetween it a b v = a + (b-a) * n
    where
        n = interp it v


interpBetween3 :: InterpolationType -> Vec3f -> Vec3f -> Float -> Vec3f
interpBetween3 it (x,y,z) (x',y',z') v = (interpBetween it x x' v, interpBetween it y y' v, interpBetween it z z' v)
sampleColorRamp :: ColorRamp -> Float -> Vec3f
sampleColorRamp (it, stops) x =
    let x' =  clamp 0 1 x
        go ((p1, c1):(p2, c2):rest)
            | x' <= p2 = interpBetween3 it c1 c2 $ normalRange (p1,p2) x'
            | otherwise = go ((p2, c2):rest)
        go [(_, c)] = c
        go [] = (0,0,0)
    in go stops

--------------------------------------------
-------- Example transfer functions --------
--------------------------------------------

---- Orion Nebula Style ---- This is shit
orionEmissionCR :: ColorRamp
orionEmissionCR = (SmoothStep,
    [ (0.00, (0.05, 0.02, 0.20))   -- very deep space purple
    , (0.25, (0.10, 0.05, 0.30))   -- dark violet
    , (0.50, (0.4, 0.12, 0.6))    -- vibrant purple
    , (0.75, (1.5, 0.6, 1.35))      -- bright pink highlights
    , (1.00, (2.0, 1.6, 2.0))      -- pale glowing lavender
    ])

orionDustCR :: ColorRamp
orionDustCR = (SmoothStep,
    [ (0.0, (0.0, 0.0, 0.0))
    , (1.0, (0.2, 0.1, 0.1))   -- faint reddish dust
    ])
orionTF :: TransferFunc
orionTF x =
    ( sampleColorRamp orionDustCR v
    , sampleColorRamp orionEmissionCR v
    , interp EaseOutCubic v
    )
    where v = x

---- Flame ----
flameCR :: ColorRamp
flameCR = (SmoothStep,
    [ (0.0, (0.3, 0.0, 0.0))
    , (0.3, (1.0, 0.3, 0.0))
    , (0.7, (1.0, 0.8, 0.2))
    , (1.0, (1.0, 1.0, 1.0))
    ])

flameTF :: TransferFunc
flameTF v = (
        (0,0,0),
        sampleColorRamp flameCR v,
        interp SmoothStep v
    )
    where glow = interp EaseInCubic v


---- Testing functions
interiorRedGlow :: TransferFunc
interiorRedGlow v = (
    (1,1,1), -- White cloud
    (glow*15, glow*5, glow*4.5),
    interp SmoothStep v)
    where glow = interp EaseInCubic v
linearTransfer :: TransferFunc
linearTransfer v = ((v,v,v), (v,v,v), v)

unitTransfer :: TransferFunc
unitTransfer _ = ((1,1,1), (1,1,1), 1.0)