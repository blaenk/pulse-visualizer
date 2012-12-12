import Control.Monad (forever)

import Visualizer.Graphics
import Visualizer.Audio

main :: IO ()
main = do
  source <- connectPulse

  -- initialize GLFW and extract the GPUResource which
  -- contains the shader information and geometry
  gpuResource <- initGLFW source

  forever $ do
    -- read audio data
    spectrum <- readSamples source
    -- draw scene
    renderVisualization gpuResource spectrum
