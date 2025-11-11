module Types where

import Data.Word (Word8, Word64)
import Data.Bits

-- Placing renderer types here to avoid circular imports
---- Volume ----
data Volume = Sphere | Cloud NoiseGenerator Int
data LightType = Point | Directional
type LightSource = (LightType, Vec3f, Vec3f)

---- NoiseGenerator ----
type NoiseSampler = Vec3f -> Float -- Maybe remove type, kinda redundant i had other plans  
type NoiseGenerator = Int -> NoiseSampler
-- interp type, start, end
data FallOff = Distance InterpolationType Float Float
             | Cube     InterpolationType Float Float

---- Transfer ----      
data InterpolationType = Linear | SmoothStep | EaseInCubic | EaseOutCubic
-- (color, emission, opacity)
type TransferData = (Vec3f, Vec3f, Float)
type CompositeTransferFunc = Float -> (TransferData, TransferData, TransferData)
type TransferFunc = Float -> TransferData
type ColorRamp = (InterpolationType, [(Float, Vec3f)])

-- Vector Types 
type Vec2i = (Int, Int)
type Vec3i = (Int, Int, Int)
type Vec2f = (Float, Float)
type Vec3f = (Float, Float, Float)
type Vec4f = (Float, Float, Float, Float)
type Color3 = (Word8, Word8, Word8)

clamp :: Float -> Float -> Float -> Float
clamp a b = max a . min b

clamp3 :: Float -> Float -> Vec3f -> Vec3f
clamp3 a b (x,y,z) = (clamp a b x, clamp a b y, clamp a b z)

scale3 :: Float -> Vec3f -> Vec3f
scale3 s (x,y,z) = (x*s,y*s,z*s)

sub3 :: Vec3f -> Vec3f -> Vec3f
sub3 (a,b,c) (x,y,z) = (a-x,b-y, c-z)

add3 :: Vec3f -> Vec3f -> Vec3f
add3 (a,b,c) (x,y,z) = (a+x,b+y, c+z)

dot :: Vec3f -> Vec3f -> Float
dot (a,b,c) (x,y,z) = a*x + b*y + c*z

magnitude3 :: Vec3f -> Float
magnitude3 (x,y,z) = sqrt (x*x+y*y+z*z)

normalise3 :: Vec3f -> Vec3f
normalise3 (x,y,z) = (x/m,y/m,z/m)
    where m = magnitude3 (x,y,z)

-- From Vec3f [0,1] to Color3 [0,255]
toColor :: Vec3f -> Color3
toColor (x,y,z) = (f x, f y, f z)
    where f = fromIntegral . round . (* 255) . clamp 0 1

packColor :: Color3 -> Word64
packColor (r, g, b) = (fromIntegral r `shiftL` 24)
    .|. (fromIntegral g `shiftL` 16)
    .|. (fromIntegral b `shiftL` 8)
    .|. 255

-- Other helpers --
mapRange :: Vec2f -> Vec2f -> Float -> Float
mapRange (in_s, in_e) (out_s, out_e) v = (v - in_s) * (out_e - out_s) / (in_e - in_s) + out_s

-- Map range to [0,1]
normalRange :: Vec2f -> Float -> Float
normalRange (in_s, in_e) v = (v - in_s) / (in_e - in_s) 