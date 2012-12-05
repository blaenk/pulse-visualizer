import Control.Monad (forM_, unless)

import qualified Graphics.UI.SDL as SDL

import Visualizer.Graphics
import Visualizer.Audio

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode 640 480 32 [SDL.HWSurface, SDL.DoubleBuf]
    SDL.setCaption "Visualizer" []

    -- mono instead of stereo? what would stereo entail?
    -- probably in stereo the data is interleaved?
    -- note: attempting to drop sampling rate to 4096 slows everything down, presumably this prevents
    --       samples read from being 'pass-thru'
    source <- connectPulse

    loop screen source

  where loop screen source = do
          quit <- whileEvents
          if quit then do
            -- close the connection
            disconnectPulse source
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

