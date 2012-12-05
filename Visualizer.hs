{-
  To find pulseaudio source monitors:
    pacmd list-sources | awk '/name:.+monitor/'
  To find ALSA recording devices:
    arecord -l
  For find ALSA recording device aliases:
    arecord -L

  packages used:
   fft
   SDL
   SDL-gfx
   pulse-simple
-} 

-- apparently the else needs indenting. see http://hackage.haskell.org/trac/haskell-prime/wiki/DoAndIfThenElse

-- sound
import qualified Sound.Pulse.Simple as Pulse
import qualified Math.FFT as FFTW
import qualified Data.Array.CArray as CA
import qualified Data.Array.IArray as IA
import Data.Complex
import Data.Int (Int16) -- S16 in SampleSpec

-- graphics
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL.P

-- misc
import Control.Monad (forever, forM_, unless, void)
import Data.Word (Word8, Word32)
import Data.Bits (shiftL, (.|.))

type Buffer = [Int16]
type ComplexArray = CA.CArray Int (Complex Double)

defaultMonitor :: Maybe String
defaultMonitor = Just "alsa_output.pci-0000_00_1b.0.analog-stereo.monitor"

bufferSize :: Int
bufferSize = 4096

-- for sdl_gfx primitives; SDL.mapRGBA seems to be broken
-- just a convenience method to call with decimal integer values: color 255 255 255 255
-- can just use SDL.Pixel 0xFFFFFFFF
color :: Word8 -> Word8 -> Word8 -> Word8 -> SDL.Pixel
color r g b a =
  SDL.Pixel $ (shiftL (fromIntegral r) 24) .|.
              (shiftL (fromIntegral g) 16) .|.
              (shiftL (fromIntegral b) 8) .|.
              (fromIntegral a)

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode 640 480 32 [SDL.HWSurface, SDL.DoubleBuf]
    SDL.setCaption "Visualizer" []

    -- mono instead of stereo? what would stereo entail?
    -- probably in stereo the data is interleaved?
    -- note: attempting to drop sampling rate to 4096 slows everything down, presumably this prevents
    --       samples read from being 'pass-thru'
    source <- Pulse.simpleNew Nothing "Visualizer" Pulse.Record defaultMonitor "A PulseAudio visualizer written in Haskell"
               (Pulse.SampleSpec (Pulse.S16 Pulse.LittleEndian) 44100 1) Nothing Nothing

    loop screen source

  where loop screen source = do
          quit <- whileEvents
          if quit then do
            -- close the connection
            Pulse.simpleFree source
          else do
            -- clear the screen
            SDL.fillRect screen (Just (SDL.Rect 0 0 640 480)) (SDL.Pixel 0x00000000) -- (color 0x00 0x00 0x00 0x00)
            -- read audio data
            readSamples source [(renderBars screen)]
            -- print Pulse latency
            -- Pulse.simpleGetLatency source >>= putStrLn . show
            -- flip the buffers
            SDL.flip screen
            -- loop
            loop screen source
        whileEvents = do
          event <- SDL.pollEvent
          case event of
            SDL.Quit -> return True
            SDL.NoEvent -> return False
            _ -> whileEvents

readSamples :: Pulse.Simple -> [(Buffer -> IO ())] -> IO ()
readSamples source actions =
  do buffer <- Pulse.simpleRead source bufferSize :: IO Buffer
     let frequencies = polarMagnitudes $ frequencySpectrum buffer

     -- pass the buffer (later will be just the fourier transform) to every action that wants it
     forM_ actions $ \action -> action buffer

-- general purpose sound generating function
sound :: Double -> Int -> Double -> Int16 -> [Int16]
sound freq samples len volume = take (round $ len * (fromIntegral samples)) $  -- take out desired number of samples
                         map (round . (* fromIntegral volume)) $ -- scale by the volume
                         map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..] -- generate sample

-- if speed is an issue:
     -- try 'real fft' (p. 239 of book)
     -- Pulse' simpleReadRaw
     -- CArray's unsafeByteStringToCArray
     -- IArray's // to modify indices in-place
frequencySpectrum :: Buffer -> ComplexArray
frequencySpectrum buffer =
  -- calculate the fourier transform
  let len = length buffer
      complex = (CA.listArray (0, len - 1) $ map (\sample -> (fromIntegral sample) :+ 0) buffer) :: ComplexArray
  in FFTW.dft complex

polarMagnitudes :: ComplexArray -> CA.CArray Int Double
polarMagnitudes spectrum =
  CA.sliceWith region region polarConversion spectrum
  where polarConversion (r :+ i) = sqrt (r^2 + i^2)
        region = (0, (bufferSize - 1)) -- we only need up to N / 2 - 1

fiveBins :: CA.CArray Int Double -> [Double]
fiveBins polars = undefined

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

normalize :: Buffer -> Int
normalize buffer = let ints = map (fromIntegral . abs) buffer :: [Int]
                   in round ((fromIntegral (sum ints `div` fromIntegral (length buffer))) / 32767 * 100)

-- print volume bar to terminal
printAverageAmplitude :: Buffer -> IO ()
printAverageAmplitude buffer =
  do putStrLn (replicate (normalize buffer) '*')

