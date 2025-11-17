-- PLEASE VISIT FOR BETTER CODE, MORE RENDERS AND A COOL README
-- https://github.com/AstroTurffx/nebula_renderer
-- https://github.com/AstroTurffx/nebula_renderer
-- https://github.com/AstroTurffx/nebula_renderer
-- https://github.com/AstroTurffx/nebula_renderer
-- https://github.com/AstroTurffx/nebula_renderer

module Main where
------------------------
-------- Imports -------
------------------------
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.ByteString as B
import qualified Numeric.Noise as Noise
import Foreign.ForeignPtr
import Foreign.Storable (pokeElemOff)
import Data.Array.IO
import Control.Concurrent
import Control.Monad (forM_)
import Data.Word (Word8, Word64)
import Foreign (pokeArray)
import GHC.IO (unsafePerformIO)
import System.Exit (exitSuccess)
import Debug.Trace (trace)
import Data.Traversable (forM)
import GHC.Base (when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-------------------------
-------- Types.hs -------
-------------------------
data Volume = Sphere
            | Cloud FallOff NoiseGenerator Int
            | CompositeCloud FallOff NoiseGenerator Int Int
data LightType = Point | Directional
type LightSource = (LightType, Vec3f, Vec3f)
type NoiseSampler = Vec3f -> Float
type NoiseGenerator = Int -> NoiseSampler
data FallOff = Distance InterpolationType Float Float
             | Cube     InterpolationType Float Float
data InterpolationType = Linear | SmoothStep | EaseInCubic | EaseOutCubic
type TransferData = (Vec3f, Vec3f, Float)
type TransferFunc = Float -> TransferData
type ColorRamp = (InterpolationType, [(Float, Vec3f)])
type Vec2i = (Int, Int)
type Vec3i = (Int, Int, Int)
type Vec2f = (Float, Float)
type Vec3f = (Float, Float, Float)
type Vec4f = (Float, Float, Float, Float)
type Color3 = (Word8, Word8, Word8)
type Color4 = (Word8, Word8, Word8, Word8)

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
toColor3 :: Vec3f -> Color3
toColor3 (x,y,z) = (f x, f y, f z)
    where f = fromIntegral . round . (* 255) . clamp 0 1
toColor4 :: Vec4f -> Color4
toColor4 (x,y,z,a) = (f x, f y, f z, f a)
    where f = fromIntegral . round . (* 255) . clamp 0 1
expose :: Float -> Vec4f -> Vec4f
expose x (r,g,b,a) = (r*x,g*x,b*x,a)
mapRange :: Vec2f -> Vec2f -> Float -> Float
mapRange (in_s, in_e) (out_s, out_e) v = (v - in_s) * (out_e - out_s) / (in_e - in_s) + out_s
normalRange :: Vec2f -> Float -> Float
normalRange (in_s, in_e) v = (v - in_s) / (in_e - in_s)

------------------------
-------- Main.hs -------
------------------------
exposure = 1

selectedNoise :: NoiseGenerator
selectedNoise = toNoiseGenerator noise
    where noise = Noise.fractal3 Noise.defaultFractalConfig{Noise.octaves = 5} Noise.perlin3

selectedFalloff :: FallOff
selectedFalloff = Cube SmoothStep 0.6 2

selectedVolume :: Volume
selectedVolume = CompositeCloud selectedFalloff selectedNoise 9 2

selectedTransfers :: [TransferFunc]
selectedTransfers = [o3TF, haTF]

selectedLights :: [LightSource]
selectedLights = [ (Point, (0.0, 0.0, 0.0), (25,25,25)), (Directional, (0.5, 2.0, 0.5), (50,0,50)) ]
-- Constants --
width, height, fps, numWorkers :: Int
width  = 256
height = 256
fps = 10
numWorkers = 4

type World = (PixelBuffer, Picture)
type PixelBuffer = ForeignPtr Word8

main :: IO ()
main = do
    putStrLn "===================================="
    putStrLn "Please visit the github repo for example renders and other good stuff"
    putStrLn "https://github.com/AstroTurffx/nebula_renderer"
    putStrLn "===================================="

    let numBytes = width * height * 4

    -- lock   <- newMVar ()
    buffer <- mallocForeignPtrArray numBytes :: IO PixelBuffer

    -- Initialize to black
    withForeignPtr buffer $ \ptr ->
        forM_ [0..width*height*4-1] $ \i ->
            pokeElemOff ptr i (0 :: Word8)
    
    let pic = bitmapOfForeignPtr width height
                    (BitmapFormat TopToBottom PxRGBA)
                    buffer
                    False

    forM_ [0..(numWorkers-1)] $ \i -> forkIO (worker i buffer)
    playIO (InWindow "Nebula Renderer" (width, height) (100, 100))
        black
        fps
        (buffer, pic)
        drawBuffer
        handleEvent
        update

handleEvent :: Event -> World -> IO World
handleEvent _ = return

update :: Float -> World -> IO World
update _ = return

drawBuffer :: World -> IO Picture
drawBuffer (buffer, pic) = return $ pic

worker :: Int -> PixelBuffer -> IO ()
worker i buffer = do
    let n = height `div` numWorkers
        start = i * n
        end   = (i+1) * n - 1

    putStrLn $ "Worker " ++ show (i+1) ++ ": Working from " ++ show start ++ " to " ++ show end
    startTime <- getCurrentTime

    forM_ [start..end] $ \y -> do
        rowPixels <- forM [0..width-1] $ \x ->
            renderPixel (x, y)
        withForeignPtr buffer $ \ptr -> do
            forM_ (zip [0..] rowPixels) $ \(x, (!r,!g,!b,!a)) -> do
                let pixelIndex = (y * width + x) * 4
                pokeElemOff ptr (pixelIndex + 0) r
                pokeElemOff ptr (pixelIndex + 1) g
                pokeElemOff ptr (pixelIndex + 2) b
                pokeElemOff ptr (pixelIndex + 3) a

        when (y `mod` 32 == 0 && y /= start) $
            putStrLn $ "Worker " ++ show (i+1) ++ ": Finished row " ++ show y
    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
    putStrLn $ "Worker " ++ show (i+1) ++ ": Completed in " ++ show elapsed

renderPixel :: Vec2i -> IO Color4
renderPixel (x,y) = do
    return . toColor4 . expose exposure $ orthoRayVolume selectedVolume selectedTransfers selectedLights (fx, fy)
    where
        fl = fromIntegral
        fx = 2 * (fl x / fl width)  - 1.0 -- map [0, width] -> [-1,1]
        fy = 2 * (fl y / fl height) - 1.0 -- map [0, height] -> [-1,1]


---------------------------
-------- Volume.hs --------
---------------------------
dz, nscale, cutoff, reflectivity :: Float
dz = 2.0 / 16.0
nscale = 1.3
cutoff = 0.48
reflectivity = 0.6

warpFreq, warpStrength :: Float
warpFreq = 0.06
warpStrength = 40

lightSampleSteps :: Int
lightSampleSteps = 4

sampleVolume :: Volume -> Vec3f -> [Float]
sampleVolume Sphere (x,y,z) = [exp (- ((r * 3) ^ 2))]
    where r = sqrt (x*x + y*y + z*z)

sampleVolume (Cloud falloff generator seed) (x,y,z)
    | n <= cutoff = [0.0]
    | otherwise   = [normalRange (cutoff, 1) n]
    where n = fallOffValue falloff (x,y,z) * generator seed (scale3 nscale (x,y,z))

sampleVolume (CompositeCloud falloff generator seed numElements) pos = foldr1 (++) [
    let innerNoise = generator (seed+i)
        warpPos = add3 pos $ warpSample3 warpStrength warpFreq innerNoise pos
        density = fallOffValue falloff pos * generator seed (scale3 nscale warpPos)
    in [clamp 0 1 . normalRange (cutoff, 1) $ density]
    | i <- [1..numElements]]

orthoRayVolume :: Volume -> [TransferFunc] -> [LightSource] -> Vec2f -> Vec4f
orthoRayVolume volume tfs lightSources (x, y) = go (-1) (0, 0, 0, 0)
    where
        transmisisonMult = 2
        go z (rAcc, gAcc, bAcc, tAcc)
            | z > 1 || tAcc >= 0.99 = (rAcc, gAcc, bAcc, tAcc)
            | otherwise =
                let ds = sampleVolume volume (x, y, z)
                    ((r,g,b), (er,eg,eb), opacity) = multiTransfer $ zip tfs ds
                    alpha = opacity * dz 

                    -- How much of this color will add to the accumulated color
                    transmission = (1 - tAcc) * alpha * transmisisonMult
                    viewRay = (0,0,1) -- orthgraphic view therefore constant ray
                    (sr, sg, sb) = foldr1 add3 [ incomingLightAt l volume tfs (x,y,z) viewRay | l <- lightSources ]

                    -- add emitted + scattered light
                    rAcc' = rAcc + transmission * (r * sr + er)
                    gAcc' = gAcc + transmission * (g * sg + eg)
                    bAcc' = bAcc + transmission * (b * sb + eb)
                    tAcc' = tAcc + transmission
                in go (z + dz) (rAcc', gAcc', bAcc', tAcc')

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

    
-----------------------------------
-------- NoiseGenerator.hs --------
-----------------------------------
toNoiseGenerator :: Noise.Noise3 Float -> NoiseGenerator
toNoiseGenerator noise = foo
    where
        foo :: NoiseGenerator
        foo seed (x,y,z) = (1 + Noise.noise3At noise (fromIntegral seed) x y z) / 2

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
    where r = maximum [abs x, abs y, abs z]

warpSample3 :: Float -> Float -> NoiseSampler -> Vec3f -> Vec3f
warpSample3 strength freq sampler pos = 
    let (x,y,z) = scale3 freq pos
        out = ( sampler (x + 2.0, y + 0.1, z + 7.9)
            , sampler (x + 5.3, y + 1.3, z + 0.5)
            , sampler (x + 1.7, y + 9.2, z + 2.8)
            ) -- just adding arbitary values
    in scale3 strength out

-----------------------------
-------- Transfer.hs --------
-----------------------------
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