{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Types
import Volume
import NoiseGenerator
import Transfer

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


-- Parameters -- 
exposure = 1

selectedNoise :: NoiseGenerator
selectedNoise = toNoiseGenerator noise
    where noise = Noise.fractal3 Noise.defaultFractalConfig{Noise.octaves = 5} Noise.perlin3

selectedFalloff :: FallOff
selectedFalloff = Cube SmoothStep 0.6 2

selectedVolume :: Volume
-- selectedVolume = Cloud selectedFalloff selectedNoise 10
selectedVolume = CompositeCloud selectedFalloff selectedNoise 9 3
-- selectedVolume = Sphere

selectedTransfers :: [TransferFunc]
selectedTransfers = [haTF, o3TF]
-- selectedTransfers = [tfFromCR o3CR]
-- selectedTransfers = [noGlowTransfer]
-- selectedTransfers = [linearTransfer]

selectedLights :: [LightSource]
-- selectedLights = [(Directional, (2.0, 0.5, 0.5), (0,0,50))]
-- selectedLights = [(Directional, (0.0, 0.0, -1.0), (0,0,25))]
-- selectedLights = [ (Point, (0.0, 0.0, 0.0), (25,25,25)) ]
selectedLights = [ (Point, (0.0, 0.0, 0.0), (10,25,75))
                 , (Point, (0.5, 0.5, 0.5), (75,25,25))
                 , (Point, (-0.5, -0.5, -0.5), (75,35,35))
                 ]

-- Constants --
width, height, fps, numWorkers, delay :: Int
width  = 512
height = 512
fps = 10
numWorkers = 8
delay = 5000


-- User Interface --
type Lock = MVar ()
main :: IO ()
main = do
    let numBytes = width * height * 4

    -- lock   <- newMVar ()
    buffer <- mallocForeignPtrArray numBytes :: IO PixelBuffer

    -- Initialize to black
    withForeignPtr buffer $ \ptr ->
        forM_ [0..width*height*4-1] $ \i ->
            pokeElemOff ptr i (0 :: Word8)

    bgp <- loadBMP "./background.bmp"
    let sx = fromIntegral width / 512.0
        sy = fromIntegral width / 512.0
        bg = Graphics.Gloss.Interface.IO.Game.scale sx sy bgp
        pic = bitmapOfForeignPtr width height
                    (BitmapFormat TopToBottom PxRGBA)
                    buffer
                    False

    forM_ [0..(numWorkers-1)] $ \i -> forkIO (worker i buffer)
    playIO (InWindow "Nebula Renderer" (width, height) (100, 100))
        black
        fps
        (buffer, pic, bg)
        drawBuffer
        handleEvent
        update

type World = (PixelBuffer, Picture, Picture)
handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey KeyEsc) _ _ _) _ = unsafePerformIO exitSuccess -- For profiling
handleEvent _ x = return x

update :: Float -> World -> IO World
update _ = return

-- Rendering --
type PixelBuffer = ForeignPtr Word8

drawBuffer :: World -> IO Picture
drawBuffer (buffer, pic, bg) = return $ pictures [ bg, pic ]

worker :: Int -> PixelBuffer -> IO ()
worker i buffer = do
    let n = height `div` numWorkers
        start = i * n
        end   = (i+1) * n - 1

    putStrLn $ "Worker " ++ show (i+1) ++ ": Working from " ++ show start ++ " to " ++ show end
    startTime <- getCurrentTime

    forM_ [start..end] $ \y -> do
        -- Render row
        rowPixels <- forM [0..width-1] $ \x ->
            renderPixel (x, y)

        -- Write row to buffer
        withForeignPtr buffer $ \ptr -> do
            forM_ (zip [0..] rowPixels) $ \(x, (!r,!g,!b,!a)) -> do
                let pixelIndex = (y * width + x) * 4
                pokeElemOff ptr (pixelIndex + 0) r
                pokeElemOff ptr (pixelIndex + 1) g
                pokeElemOff ptr (pixelIndex + 2) b
                pokeElemOff ptr (pixelIndex + 3) a

        when (y `mod` 64 == 0 && y /= start) $
            putStrLn $ "Worker " ++ show (i+1) ++ ": Finished row " ++ show y
    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
    putStrLn $ "Worker " ++ show (i+1) ++ ": Completed in " ++ show elapsed


-- render :: PixelBuffer -> IO ()
-- render buffer = do
--     forM_ [0 .. height-1] $ \y -> do
--         forM_ [0 .. width-1] $ \x -> do
--             writeArray buffer (x,y) $ renderPixel (x,y)
--         threadDelay 1000 -- Allow gloss to render and catch up? i don't really like this tbh

renderPixel :: Vec2i -> IO Color4
renderPixel (x,y) = do
    return . toColor4 . expose exposure $ orthoRayVolume selectedVolume selectedTransfers selectedLights (fx, fy)
    where
        fl = fromIntegral -- I hate how long this function is
        -- Screen space to world space
        fx = 2 * (fl x / fl width)  - 1.0 -- map [0, width] -> [-1,1]
        fy = 2 * (fl y / fl height) - 1.0 -- map [0, height] -> [-1,1]
