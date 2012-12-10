module Visualizer.Graphics (
  renderBars,
  printPolars
) where

-- graphics
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL.P

-- misc
import qualified Data.Array.CArray as CA
import qualified Data.Array.IArray as IA
import Data.Bits (shiftL, (.|.))
import Data.Word (Word8, Word32)
import Visualizer.Audio (Buffer, Polars, Sample, Spectrum)
import Control.Monad (void)
import Data.Int (Int16) -- S16 in SampleSpec

renderBars :: SDL.Surface -> Polars -> IO ()
renderBars surface spectrum =
  -- renderBar takes coords in SDL conventional coordinates
  do mapM_ (uncurry $ renderBar surface) $ zip (CA.elems spectrum) (barSpan [10, 40, 70, 100, 130, 160, 190, 220])
  where barSpan points = map (\point -> (point, (480 - 10))) points

-- render volume bar in SDL
renderBar :: SDL.Surface -> Double -> (Int, Int) -> IO ()
renderBar surface level (sdl_x, sdl_y) =
  -- the SDL.Rect box accepts is x1 y1 x2 y2 where 1's are top right, 2's are bottom left
  do void $ SDL.P.box surface (SDL.Rect x y w h) (SDL.Pixel 0xFFFFFFFF) -- (color 255 255 255 255)
  where x = sdl_x + 20
        y = sdl_y
        w = sdl_x
        h = y - (round normalized)
        normalized = level / (fromIntegral (maxBound :: Int16))

{- 
-- print volume bar to terminal
printAverageAmplitude :: Buffer -> IO ()
printAverageAmplitude buffer =
  do putStrLn (replicate (normalize buffer) '*')
-}

printPolars :: Polars -> IO ()
printPolars polars =
  do putStrLn $ show polars

-- for sdl_gfx primitives; SDL.mapRGBA seems to be broken
-- just a convenience method to call with decimal integer values: color 255 255 255 255
-- can just use SDL.Pixel 0xFFFFFFFF
color :: Word8 -> Word8 -> Word8 -> Word8 -> SDL.Pixel
color r g b a =
  SDL.Pixel $ (shiftL (fromIntegral r) 24) .|.
              (shiftL (fromIntegral g) 16) .|.
              (shiftL (fromIntegral b) 8) .|.
              (fromIntegral a)

