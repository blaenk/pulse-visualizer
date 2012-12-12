import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)

import Data.Bits ((.|.))
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (forever, forM_, unless)

import Visualizer.Graphics

import Visualizer.Audio
import qualified Sound.Pulse.Simple as Pulse

initGL :: IO ()
initGL = do
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  glClearDepth 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

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

drawScene :: IO ()
drawScene = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity

  glTranslatef (-1.5) 0 (-6.0) --Move left 1.5 Units and into the screen 6.0

  glColor3f 1 1 1
  glBegin gl_QUADS
  glVertex3f (-0.5) 0.5 0
  glVertex3f 0.5 0.5 0
  glVertex3f 0.5 (-0.5) 0
  glVertex3f (-0.5) (-0.5) 0
  glEnd

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

main :: IO ()
main = do
  -- mono instead of stereo? what would stereo entail?
  -- probably in stereo the data is interleaved?
  -- note: attempting to drop sampling rate to 4096 slows everything down, presumably this prevents
  --       samples read from being 'pass-thru'
  source <- connectPulse

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
  GLFW.setWindowRefreshCallback drawScene
  GLFW.setWindowSizeCallback resizeScene
  GLFW.setKeyCallback (keyPressed source)
  GLFW.setWindowCloseCallback (shutdown source)

  initGL

  forever $ do
    -- read audio data
    -- readSamples source [(renderBars screen)]
    -- draw scene
    drawScene
    -- swap buffers
    GLFW.swapBuffers
