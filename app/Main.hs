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


-- Parameters -- 
exposure = 1.0

selectedNoise :: NoiseGenerator
selectedNoise = (applyFallOff (Distance SmoothStep 1.5 4.5) . warp3) noise
    where noise = Noise.fractal3 Noise.defaultFractalConfig{Noise.octaves = 5} Noise.perlin3

selectedVolume :: Volume
selectedVolume = Cloud selectedNoise 10
-- selectedVolume = Sphere

selectedTransfer :: TransferFunc
-- selectedTransfer = orionTF
selectedTransfer = linearTransfer

selectedLights :: [LightSource]
-- selectedLights = [(Directional, (2.0, 0.5, 0.5), (0,0,50))]
-- selectedLights = [(Directional, (0.0, 0.0, -1.0), (0,0,25))]
selectedLights = [(Point, (0.0, 0.0, 0.0), (10,20,20))]

-- Constants --
width, height, fps, numWorkers, delay :: Int
width  = 512
height = 512
fps = 10
numWorkers = 1
delay = 15000


-- User Interface --
type Lock = MVar ()
main :: IO ()
main = do
    let numPixels = width * height

    lock   <- newMVar ()
    buffer <- newArray (0,numPixels) (0,0,0) :: IO PixelBuffer

    forM_ [0..(numWorkers-1)] $ \i -> forkIO (worker i buffer lock)
    playIO (InWindow "Nebula Renderer" (width, height) (100, 100))
        black
        fps
        (lock, buffer)
        drawBuffer
        handleEvent
        update


handleEvent :: Event -> (Lock, PixelBuffer) -> IO (Lock, PixelBuffer)
handleEvent _ = return

update :: Float -> (Lock, PixelBuffer) -> IO (Lock, PixelBuffer)
update _ = return

-- Rendering --
type PixelBuffer = IOArray Int Color3

drawBuffer :: (Lock, PixelBuffer) -> IO Picture
drawBuffer (lock, buffer) = do
    -- allocate a temporary ForeignPtr Word64 (RGBA per pixel)
    let numPixels = width * height
    fp <- mallocForeignPtrArray (numPixels*4) :: IO (ForeignPtr Word8)

    -- Context with lock and foreign ptr
    withMVar lock $ \_ -> do
        withForeignPtr fp $ \ptr -> do
            forM_ [0..numPixels-1] $ \i -> do
                (r,g,b) <- readArray buffer i
                pokeElemOff ptr (i*4)   r
                pokeElemOff ptr (i*4+1) g
                pokeElemOff ptr (i*4+2) b
                pokeElemOff ptr (i*4+3) 255

    return $ bitmapOfForeignPtr width height (BitmapFormat TopToBottom PxRGBA) fp False



worker :: Int -> PixelBuffer -> Lock -> IO ()
worker i buffer lock = do
    forM_ [start..end] $ \y -> do
        forM_ [0 .. width-1] $ \x -> do
            let i = y*width + x
            c <- renderPixel (x,y)
            withMVar lock $ \_ -> do -- Add pixel buffer with the lock
                writeArray buffer i c
        threadDelay delay -- Allow gloss to render and catch up? i don't really like this tbh but its the only way i know
    where
        n = height `div` numWorkers
        start = i * n
        end   = (i+1) * n - 1

-- render :: PixelBuffer -> IO ()
-- render buffer = do
--     forM_ [0 .. height-1] $ \y -> do
--         forM_ [0 .. width-1] $ \x -> do
--             writeArray buffer (x,y) $ renderPixel (x,y)
--         threadDelay 1000 -- Allow gloss to render and catch up? i don't really like this tbh

renderPixel :: Vec2i -> IO Color3
renderPixel (x,y) = return . toColor . scale3 exposure $ orthoRayVolume selectedVolume selectedTransfer selectedLights (fx, fy)
    where
        fl = fromIntegral -- I hate how long this function is
        -- Screen space to world space
        fx = 2 * (fl x / fl width)  - 1.0 -- map [0, width] -> [-1,1]
        fy = 2 * (fl y / fl height) - 1.0 -- map [0, height] -> [-1,1]
