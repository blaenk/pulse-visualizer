module Visualizer.Audio (
  connectPulse,
  disconnectPulse,
  Buffer,
  Polars,
  Spectrum,
  Sample,
  defaultMonitor,
  bufferSize,
  readSamples
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

type Sample = Int16
type Buffer = [Int16]
type RealData = CA.CArray Int Double
type Spectrum = CA.CArray Int (Complex Double)
type Polars = CA.CArray Int Double

defaultMonitor :: Maybe String
defaultMonitor = Just "alsa_output.pci-0000_00_1b.0.analog-stereo.monitor"

bufferSize :: Int
bufferSize = 4096

connectPulse = Pulse.simpleNew Nothing "Visualizer" Pulse.Record defaultMonitor "A PulseAudio visualizer written in Haskell" (Pulse.SampleSpec (Pulse.S16 Pulse.LittleEndian) 44100 1) Nothing Nothing
disconnectPulse = Pulse.simpleFree

readSamples :: Pulse.Simple -> [(Polars -> IO ())] -> IO ()
readSamples source actions =
  do buffer <- Pulse.simpleRead source bufferSize :: IO Buffer
     let frequencies = polarMagnitudes $ frequencySpectrum buffer

     -- pass the spectrum
     forM_ actions $ \action -> action frequencies

bufferToRealData :: Buffer -> RealData
bufferToRealData buffer =
  (CA.listArray (0, len - 1) (map fromIntegral buffer :: [Double])) :: RealData
  where len = length buffer

polarMagnitudes :: Spectrum -> Polars
polarMagnitudes spectrum =
  CA.amap polarConversion spectrum
  where polarConversion (r :+ i) = sqrt (r^2 + i^2)

-- if speed is an issue:
     -- try 'real fft' (p. 239 of book)
     -- Pulse' simpleReadRaw
     -- CArray's unsafeByteStringToCArray
     -- IArray's // to modify indices in-place
frequencySpectrum :: Buffer -> Spectrum
frequencySpectrum buffer =
  -- calculate the fourier transform
  FFTW.dftRC $ bufferToRealData buffer

normalizeSpectrum :: Spectrum -> Spectrum
normalizeSpectrum spectrum =
  CA.amap normalize spectrum
  where normalize complex = ((realPart complex) / len) :+ ((imagPart complex) / len)
        len = fromIntegral $ (CA.size spectrum)

-- general purpose sound generating function
sound :: Double -> Int -> Double -> Sample -> Buffer
sound freq samples len volume = take (round $ len * (fromIntegral samples)) $  -- take out desired number of samples
                         map (round . (* fromIntegral volume)) $ -- scale by the volume
                         map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..] -- generate sample

