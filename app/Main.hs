{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket, bracket_)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class ()
import Data.IORef
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL
import SDL.Raw.Video (getCurrentVideoDriver)
import System.Mem

import Config
import St
import Ui

import Foreign.C.String
import Foreign.Ptr

main :: IO ()
main = do
  initializeAll
  appst <- processOpts >>= newIORef
  let title = "Cluster painter"
  let config =
        defaultWindow
          { windowGraphicsContext =
              OpenGLContext defaultOpenGL {glProfile = Core Normal 3 3}
          , windowResizable = True
          }
  bracket (createWindow title config) destroyWindow $ \window ->
    bracket (glCreateContext window) glDeleteContext $ \glContext ->
      bracket createContext destroyContext $ \_ ->
        bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
          $ bracket_ openGL3Init openGL3Shutdown
          $ do
              getCurrentVideoDriver >>= peekCString >>= print
              glGetString GL_VENDOR >>= peekCString . castPtr >>= print
              glGetString GL_RENDERER >>= peekCString . castPtr >>= print
              glGetString GL_VERSION >>= peekCString . castPtr >>= print
              glGetString GL_SHADING_LANGUAGE_VERSION
                >>= peekCString . castPtr
                >>= print
              swapInterval $= LateSwapTearing
              renderSetup appst
              mainLoop appst window

mainLoop appst window = loop
  where
    v2tup (V2 a b) = (a, b)
    loop = do
      ws0 <- fmap fromIntegral <$> get (windowSize window)
      finished <- handleEvents ws0
      unless finished $ do
        -- start frame
        openGL3NewFrame
        sdl2NewFrame
        newFrame
        -- rendering
        ws <- fmap fromIntegral <$> get (windowSize window)
        renderApp ws appst
        -- UI
        drawUI ws appst
        render
        openGL3RenderDrawData =<< getDrawData
        -- post-frame
        glSwapWindow window
        --performGC
        loop
    handleEvents sz = do
      ev' <- pollEventWithImGui
      case ev' of
        Nothing -> return False
        Just ev ->
          let ep = eventPayload ev
              next = handleEvents sz
              mouseEvent = do
                captured <- wantCaptureMouse
                unless captured $ onEvent sz ep appst
                next
              kbEvent = do
                captured <- wantCaptureKeyboard
                unless captured $ onEvent sz ep appst
                next
           in case ep of
                QuitEvent -> return True
                MouseMotionEvent _ -> mouseEvent
                MouseButtonEvent _ -> mouseEvent
                MouseWheelEvent _ -> mouseEvent
                KeyboardEvent _ -> kbEvent
                TextEditingEvent _ -> kbEvent
                TextInputEvent _ -> kbEvent
                WindowResizedEvent r -> do
                  uncurry (glViewport 0 0) . v2tup $ windowResizedEventSize r
                  onEvent sz ep appst
                  next
                _ -> do
                  onEvent sz ep appst
                  next
