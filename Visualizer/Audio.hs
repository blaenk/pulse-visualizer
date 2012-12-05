module Visualizer.Audio (
  connectPulse,
  disconnectPulse,
  Buffer,
  defaultMonitor,
  bufferSize,
  readSamples,
  normalize
) where

-- sound
import qualified Sound.Pulse.Simple as Pulse
import qualified Math.FFT as FFTW
import qualified Data.Array.CArray as CA
import qualified Data.Array.IArray as IA
import Data.Complex
import Data.Int (Int16) -- S16 in SampleSpec

-- misc
import Control.Monad (forM_)

{-
  To find pulseaudio source monitors:
    pacmd list-sources | awk '/name:.+monitor/'
  To find ALSA recording devices:
    arecord -l
  For find ALSA recording device aliases:
    arecord -L
-} 

type Buffer = [Int16]
type ComplexArray = CA.CArray Int (Complex Double)

defaultMonitor :: Maybe String
defaultMonitor = Just "alsa_output.pci-0000_00_1b.0.analog-stereo.monitor"

bufferSize :: Int
bufferSize = 4096

connectPulse = Pulse.simpleNew Nothing "Visualizer" Pulse.Record defaultMonitor "A PulseAudio visualizer written in Haskell" (Pulse.SampleSpec (Pulse.S16 Pulse.LittleEndian) 44100 1) Nothing Nothing
disconnectPulse = Pulse.simpleFree

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

normalize :: Buffer -> Int
normalize buffer = let ints = map (fromIntegral . abs) buffer :: [Int]
                   in round ((fromIntegral (sum ints `div` fromIntegral (length buffer))) / 32767 * 100)

