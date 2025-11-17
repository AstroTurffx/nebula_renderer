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

multiTransfer :: [(TransferFunc, Float)] -> TransferData
multiTransfer [] = ((0,0,0),(0,0,0),0)
multiTransfer ((tf,d):xs) =
    ( add3 color1 color2
    , add3 emission1 emission2
    , opacity1 + opacity2
    )
    where
        (color1, emission1, opacity1) = tf d
        (color2, emission2, opacity2) = multiTransfer xs

tfFromCR :: ColorRamp -> TransferFunc
tfFromCR cr v =
    ( sampleColorRamp stdRedDustCR v
    , sampleColorRamp cr v
    , 5*interp EaseOutCubic v
    )

--------------------------------------------
-------- Example transfer functions --------
--------------------------------------------

stdRedDustCR :: ColorRamp
stdRedDustCR = (SmoothStep,
    [ (0.0, (0.0, 0.0, 0.0))
    , (1.0, (0.75, 0.6, 0.6))   -- faint reddish dust
    ])

stdDustCR :: ColorRamp
stdDustCR = (SmoothStep,
    [ (0.0, (0.00, 0.00, 0.00))
    , (1.0, (0.75, 0.75, 0.75))
    ])

---- Element based ----
haCR, o3CR, s2CR, n2CR :: ColorRamp
haCR = (SmoothStep,
    [ (0.0, (0.4, 0.05, 0.05))
    , (0.5, (1.0,  0.2,  0.1))
    , (1.0, (1.0,  0.6,  0.3))
    ])
o3CR = (SmoothStep,
    [ (0.0, (0.05, 0.10, 0.20))
    , (0.5, (0.10, 0.40, 0.90))
    , (1.0, (0.80, 1.80, 2.00))
    ])
s2CR = (SmoothStep,
    [ (0.0, (0.2, 0.05, 0.2))
    , (0.5, (0.8, 0.2,  0.8))
    , (1.0, (1.0, 0.6,  1.0))
    ])
n2CR = (SmoothStep,
    [ (0.00, (0.05, 0.02, 0.00))
    , (0.50, (0.60, 0.45, 0.10))
    , (1.00, (2.00, 1.80, 0.60))
    ])

haTF, o3TF, s2TF, n2TF :: TransferFunc
haTF v = 
    ( sampleColorRamp stdDustCR v
    , scale3 0.4 $ sampleColorRamp haCR v
    , 3 * interp EaseOutCubic v
    )
o3TF v = 
    ( sampleColorRamp stdDustCR v
    , scale3 1 $ sampleColorRamp o3CR v
    , 3 * interp EaseOutCubic v
    )
s2TF v = 
    ( sampleColorRamp stdDustCR v
    , scale3 0.75 $ sampleColorRamp s2CR v
    , 5 * interp EaseOutCubic v
    )
n2TF v = 
    ( sampleColorRamp stdDustCR v
    , scale3 1.3 $ sampleColorRamp n2CR v
    , 3 * interp EaseOutCubic v
    )


---- Orion Nebula Style ---- This is shit
orionEmissionCR :: ColorRamp
orionEmissionCR = (SmoothStep,
    [ (0.00, (0.05, 0.02, 0.20)) -- red
    , (0.07, (0.80, 0.20, 0.00)) -- red
    , (0.20, (1.00, 1.00, 1.00)) -- white
    , (0.39, (0.40, 0.40, 1.00)) -- blue
    , (0.40, (0.10, 0.05, 0.30)) -- purple
    , (0.50, (0.40, 0.12, 0.60)) -- purple
    , (0.75, (1.50, 0.60, 1.35)) -- purple
    , (1.00, (2.00, 1.60, 2.00)) -- purple
    ])

orionTF :: TransferFunc
orionTF x =
    ( sampleColorRamp stdRedDustCR v
    , scale3 0.75 $ sampleColorRamp orionEmissionCR v
    , 4 * interp EaseOutCubic v
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
flameTF v =
    ( (0,0,0)
    , sampleColorRamp flameCR v
    , interp SmoothStep v
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

noGlowTransfer :: TransferFunc
noGlowTransfer v =
    ( sampleColorRamp stdDustCR v
    , (0,0,0)
    , 5*interp EaseOutCubic v
    )

test1, test2 :: TransferFunc
test1 v =((0,v,0), (0,v,0), v)
test2 v =((0,0,v), (0,0,v), v)