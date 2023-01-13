module Spaghetti.GUI.SDL
  ( initialiseWindow
  , unlessQuit
  , beginFrame
  , renderFrame
  , HasSDLWindow(..)
  , windowSize
  ) where
  
import Relude

import Control.Exception
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL2
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL hiding ( windowSize )
import qualified Data.StateVar as SV
import qualified SDL
import Optics
import Foreign.C.Types

initialiseWindow :: 
  Text 
  -> V2 Int
  -> Managed Window
initialiseWindow title size = do
  initializeAll
  let config = defaultWindow 
        { windowGraphicsContext = OpenGLContext defaultOpenGL
        , windowMode = Maximized
        , windowResizable = True
        , windowInitialSize = fmap fromIntegral size
        }
  window <- managed $ bracket (createWindow title config) destroyWindow
  glContext <- managed $ bracket (glCreateContext window) glDeleteContext
  managed $ bracket createContext destroyContext
  managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
  managed_ $ bracket_ openGL2Init openGL2Shutdown
  return window

beginFrame :: IO ()
beginFrame = do
  openGL2NewFrame
  sdl2NewFrame
  newFrame

renderFrame :: Window -> IO ()
renderFrame window = do
  glClear GL_COLOR_BUFFER_BIT
  render
  getDrawData >>= openGL2RenderDrawData
  glSwapWindow window

unlessQuit :: 
  MonadIO m 
  => m a 
  -> m ()
unlessQuit action = void $ do
  shouldQuit <- checkEvents
  unless shouldQuit (void action)
  where
    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent

class HasSDLWindow s where
  windowL :: Lens' s SDL.Window

windowSize :: 
  HasSDLWindow s
  => s
  -> IO (V2 Int)
windowSize s = fmap (\(CInt c) -> fromIntegral c) <$> (SV.get . SDL.windowSize $ s ^. windowL)