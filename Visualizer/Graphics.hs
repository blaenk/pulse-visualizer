module Visualizer.Graphics (
  renderVisualization,
  printPolars,
  initGLFW
) where

-- graphics

-- todo: migrate to GLFW neat
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU (perspective)
import qualified Graphics.GLUtil as GLUtil

-- audio types
import Visualizer.Audio (Buffer, Polars, Sample, Spectrum, disconnectPulse)
import qualified Sound.Pulse.Simple as Pulse

-- misc
import qualified Data.Array.CArray as CA
import Control.Applicative
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Foreign.Storable (sizeOf)
import System.FilePath ((</>))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)

-- GPU Resources
data ShaderResource = ShaderResource {
  vertexShader   :: GL.VertexShader,
  fragmentShader :: GL.FragmentShader,
  program        :: GL.Program,
  barNumberU     :: GL.UniformLocation,
  spectrumU      :: GL.UniformLocation,
  positionA      :: GL.AttribLocation
}

data GPUResource = GPUResource {
  vertexBuffer   :: GL.BufferObject,
  elementBuffer  :: GL.BufferObject,
  shaderResource :: ShaderResource
}

cubeBufferData :: [GL.GLfloat]
cubeBufferData = [-0.1, 0,  0.1, 1,
                   0.1, 0,  0.1, 1,
                  -0.1,  0.2,  0.1, 1,
                   0.1,  0.2,  0.1, 1,
                  -0.1, 0, -0.1, 1,
                   0.1, 0, -0.1, 1,
                  -0.1,  0.2, -0.1, 1,
                   0.1,  0.2, -0.1, 1]

cubeBufferElements :: [GL.GLuint]
cubeBufferElements = [0, 1, 2, 3, 7, 1, 5, 4, 7, 6, 2, 4, 0, 1]

loadShaderResource = do
  vertexShader   <- GLUtil.loadShader $ "shaders" </> "cube.vert"
  fragmentShader <- GLUtil.loadShader $ "shaders" </> "cube.frag"
  program        <- GLUtil.linkShaderProgram [vertexShader] [fragmentShader]

  ShaderResource vertexShader fragmentShader program
    <$> GL.get (GL.uniformLocation program "bar_number")
    <*> GL.get (GL.uniformLocation program "spectrum")
    <*> GL.get (GL.attribLocation program "position")

makeGPUResource =
  GPUResource
    <$> GLUtil.makeBuffer GL.ArrayBuffer cubeBufferData
    <*> GLUtil.makeBuffer GL.ElementArrayBuffer cubeBufferElements
    <*> loadShaderResource

setupGeometry :: GPUResource -> IO ()
setupGeometry gpuResource =
  let posn = positionA (shaderResource gpuResource)
      stride = fromIntegral $ sizeOf (undefined :: GL.GLfloat) * 4
      vad = GL.VertexArrayDescriptor 4 GL.Float stride GLUtil.offset0
  in do GL.bindBuffer GL.ArrayBuffer $= Just (vertexBuffer gpuResource)
        GL.vertexAttribPointer posn  $= (GL.ToFloat, vad)
        GL.vertexAttribArray posn    $= GL.Enabled

drawCube :: GPUResource -> GL.GLfloat -> GL.GLfloat -> IO ()
drawCube gpuResource barNumber spectrum =
  do GL.currentProgram $= Just (program (shaderResource gpuResource))
     GL.uniform (barNumberU (shaderResource gpuResource))   $= GL.Index1 (barNumber :: GL.GLfloat)
     GL.uniform (spectrumU (shaderResource gpuResource)) $= GL.Index1 (spectrum :: GL.GLfloat)

     setupGeometry gpuResource
     GL.bindBuffer GL.ElementArrayBuffer $= Just (elementBuffer gpuResource)
     GL.drawElements GL.TriangleStrip 14 GL.UnsignedInt GLUtil.offset0

-- GLFW
initGLFW :: Pulse.Simple -> IO (IORef GPUResource)
initGLFW source = do
  True <- GLFW.initialize

  let displayOptions = GLFW.defaultDisplayOptions {
    GLFW.displayOptions_width = 800,
    GLFW.displayOptions_height = 600,
    GLFW.displayOptions_numRedBits = 8,
    GLFW.displayOptions_numGreenBits = 8,
    GLFW.displayOptions_numBlueBits = 8,
    GLFW.displayOptions_numAlphaBits = 8,
    GLFW.displayOptions_numDepthBits = 1
  }

  True <- GLFW.openWindow displayOptions

  GLFW.setWindowPosition 0 0
  GLFW.setWindowTitle "PulseAudio Visualizer in Haskell"
  -- GLFW.setWindowRefreshCallback renderVisualization
  GLFW.setWindowSizeCallback resizeScene
  GLFW.setKeyCallback (keyPressed source)
  GLFW.setWindowCloseCallback (shutdown source)

  GL.shadeModel $= GL.Smooth
  GL.clearColor $= (GL.Color4 0.0 0.0 0.0 (1.0 :: GL.GLfloat))
  GL.clearDepth $= 1
  GL.depthFunc $= Just GL.Lequal
  GL.hint PerspectiveCorrection $= Nicest

  makeGPUResource >>= newIORef

-- clean this up and just pass the CA.elems spectrum since we do it anyways
-- removes dependency on CA and IA
renderBars :: GPUResource -> Polars -> IO ()
renderBars gpuResource spectrum =
  do renderBars' chosenElems 0
  where spectrumElems = (drop 1 $ CA.elems spectrum)
        len = (CA.size spectrum) - 1

        -- choose even distribution from spectrum
        chosenElems = (every (len `div` 8) spectrumElems)
        -- to normalize the spectrum data (i.e. turn it into a scaling value [0,1])
        maxSpectrum = maximum spectrumElems

        -- draw a bar then translate to the right by 'sep' units
        renderBars' :: [Double] -> Int -> IO ()
        renderBars' [] count = return ()
        renderBars' (x:xs) count = do
          renderBar (fromIntegral count) x
          renderBars' xs (count + 1)

        -- take every nth element from a list: http://stackoverflow.com/a/2028218
        every n xs = case drop (n - 1) xs of
                       (y:ys) -> y : every n ys
                       [] -> []

        -- render volume bar in SDL
        renderBar :: Double -> Double -> IO ()
        renderBar barNumber level = do
          drawCube gpuResource (realToFrac barNumber) (realToFrac normalized)
          where x = 0 -- sdl_x + 20
                y = 0 -- sdl_y
                w = 0 -- sdl_x
                h = y - (round normalized)
                normalized = level / maxSpectrum -- (fromIntegral (maxBound :: Int16))
                -- scaled = normalized * 300

renderVisualization :: IORef GPUResource -> Polars -> IO ()
renderVisualization gpuResource' polars = do
  gpuResource <- readIORef gpuResource'
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  -- GL.loadIdentity

  -- setup camera position
  GL.translate $ GL.Vector3 0 0 (-6.0 :: GL.GLfloat) --Move left 1.5 Units and into the screen 6.0

  -- render the bars
  renderBars gpuResource polars

  -- swap buffers
  GLFW.swapBuffers

resizeScene :: GLFW.WindowSizeCallback
resizeScene w 0 = resizeScene w 1
resizeScene width height = do
  GL.viewport $= (GL.Position 0 0, (GL.Size (fromIntegral width) (fromIntegral height)))
  GL.matrixMode $= Projection
  GL.loadIdentity
  GLU.perspective 45 (fromIntegral width / fromIntegral height) 0.1 100
  GL.matrixMode $= Modelview 0
  GL.loadIdentity
  GL.flush

shutdown :: Pulse.Simple -> GLFW.WindowCloseCallback
shutdown source = do
  disconnectPulse source
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return True

keyPressed :: Pulse.Simple -> GLFW.KeyCallback
keyPressed source GLFW.KeyEsc True = (shutdown source) >> return ()
keyPressed source _ _ = return ()

printPolars :: Polars -> IO ()
printPolars polars =
  do putStrLn $ show polars
