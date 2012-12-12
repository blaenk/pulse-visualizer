module Visualizer.Graphics (
  renderVisualization,
  printPolars,
  initGLFW
) where

-- graphics
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)

-- audio types
import Visualizer.Audio (Buffer, Polars, Sample, Spectrum, disconnectPulse)
import qualified Sound.Pulse.Simple as Pulse

-- misc
import qualified Data.Array.CArray as CA
import qualified Data.Array.IArray as IA
import Data.Bits (shiftL, (.|.))
import Control.Monad (void, forever, forM_, unless)
import Data.Int (Int16) -- S16 in SampleSpec
import System.Exit (exitWith, ExitCode(..))

data Rect = Rect Int Int Int Int

-- GLFW
initGLFW :: Pulse.Simple -> IO ()
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

  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

renderBars :: Polars -> IO ()
renderBars spectrum =
  -- renderBar takes coords in SDL conventional coordinates
  do mapM_ (uncurry $ renderBar) $ zip chosenElems (barSpan [10, 40, 70, 100, 130, 160, 190, 220])
  where barSpan points = map (\point -> (point, (480 - 10))) points
        spectrumElems = (drop 1 $ CA.elems spectrum)
        len = (CA.size spectrum) - 1
        chosenElems = (every (len `div` 8) spectrumElems)
        maxSpectrum = maximum spectrumElems
        -- take every nth element from a list: http://stackoverflow.com/a/2028218
        every n xs = case drop (n - 1) xs of
                       (y:ys) -> y : every n ys
                       [] -> []
        -- render volume bar in SDL
        renderBar :: Double -> (Int, Int) -> IO ()
        renderBar level (sdl_x, sdl_y) = do
          -- the SDL.Rect box accepts is x1 y1 x2 y2 where 1's are top right, 2's are bottom left
          -- do void $ SDL.P.box surface (SDL.Rect x y w h) (SDL.Pixel 0xFFFFFFFF) -- (color 255 255 255 255)

          glColor3f 1 1 1

          glBegin gl_QUADS
          glVertex3f (-1) 1    0 -- TL
          glVertex3f 1    1    0 -- TR
          glVertex3f 1    (-1) 0 -- BR
          glVertex3f (-1) (-1) 0 -- BL
          glEnd

          glFlush

          where x = sdl_x + 20
                y = sdl_y
                w = sdl_x
                h = y - (round scaled)
                normalized = level / maxSpectrum -- (fromIntegral (maxBound :: Int16))
                scaled = normalized * 300

renderVisualization :: Polars -> IO ()
renderVisualization polars = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity

  -- setup camera position
  glTranslatef (-1.5) 0 (-6.0) --Move left 1.5 Units and into the screen 6.0

  -- render the bars
  renderBars polars

  -- swap buffers
  GLFW.swapBuffers

resizeScene :: GLFW.WindowSizeCallback
resizeScene w 0 = resizeScene w 1
resizeScene width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width / fromIntegral height) 0.1 100
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

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

{- 
-- print volume bar to terminal
printAverageAmplitude :: Buffer -> IO ()
printAverageAmplitude buffer =
  do putStrLn (replicate (normalize buffer) '*')
-}

printPolars :: Polars -> IO ()
printPolars polars =
  do putStrLn $ show polars
