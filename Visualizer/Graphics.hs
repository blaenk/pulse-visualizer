module Visualizer.Graphics (
  renderBars
) where

-- graphics
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL.P

-- misc
import Data.Bits (shiftL, (.|.))
import Data.Word (Word8, Word32)
import Visualizer.Audio (Buffer, normalize)
import Control.Monad (void)

renderBars :: SDL.Surface -> Buffer -> IO ()
renderBars surface buffer =
  -- renderBar takes coords in SDL conventional coordinates
  do mapM_ (renderBar surface normalized) $ barSpan [10, 40, 70, 100, 130]
  where barSpan points = map (\point -> (point, (480 - 10))) points
        normalized = normalize buffer

-- render volume bar in SDL
renderBar :: SDL.Surface -> Int -> (Int, Int) -> IO ()
renderBar surface normalized (sdl_x, sdl_y) =
  -- the SDL.Rect box accepts is x1 y1 x2 y2 where 1's are top right, 2's are bottom left
  do void $ SDL.P.box surface (SDL.Rect x y w h) (SDL.Pixel 0xFFFFFFFF) -- (color 255 255 255 255)
  where x = sdl_x + 20
        y = sdl_y
        w = sdl_x
        h = y - normalized

-- print volume bar to terminal
printAverageAmplitude :: Buffer -> IO ()
printAverageAmplitude buffer =
  do putStrLn (replicate (normalize buffer) '*')

-- for sdl_gfx primitives; SDL.mapRGBA seems to be broken
-- just a convenience method to call with decimal integer values: color 255 255 255 255
-- can just use SDL.Pixel 0xFFFFFFFF
color :: Word8 -> Word8 -> Word8 -> Word8 -> SDL.Pixel
color r g b a =
  SDL.Pixel $ (shiftL (fromIntegral r) 24) .|.
              (shiftL (fromIntegral g) 16) .|.
              (shiftL (fromIntegral b) 8) .|.
              (fromIntegral a)

