import Control.Monad (forever)

import Visualizer.Graphics
import Visualizer.Audio

main :: IO ()
main = do
  -- mono instead of stereo? what would stereo entail?
  -- probably in stereo the data is interleaved?
  -- note: attempting to drop sampling rate to 4096 slows everything down, presumably this prevents
  --       samples read from being 'pass-thru'
  source <- connectPulse

  initGLFW source

  forever $ do
    -- read audio data
    spectrum <- readSamples source
    -- draw scene
    renderVisualization spectrum
