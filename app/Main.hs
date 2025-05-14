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
import Graphics.Gloss.Rendering
import SDL

import Config
import St
import Ui

main :: IO ()
main = do
  initializeAll
  appst <- processOpts >>= newIORef
  let title = "Cluster painter"
  let config =
        defaultWindow
          { windowGraphicsContext = OpenGLContext defaultOpenGL
          , windowResizable = True
          }
  bracket (createWindow title config) destroyWindow $ \window ->
    bracket (glCreateContext window) glDeleteContext $ \glContext ->
      bracket createContext destroyContext $ \_ ->
        bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
          $ bracket_ openGL3Init openGL3Shutdown
          $ do
              st <- initState
              swapInterval $= LateSwapTearing
              mainLoop appst st window

--mainLoop :: AppState -> State -> Window -> IO ()
mainLoop appst st window = loop
  where
    loop = do
      finished <- handleEvents
      unless finished $ do
        -- start frame
        openGL3NewFrame
        sdl2NewFrame
        newFrame
        -- rendering
        (V2 wsx wsy) <- fmap fromIntegral <$> get (windowSize window)
        renderApp wsx wsy appst >>= \(bg, pic) ->
          displayPicture (wsx, wsy) bg st 1.0 pic
        -- UI
        drawUI appst
        -- UI rendering
        render
        openGL3RenderDrawData =<< getDrawData
        -- post-frame
        glSwapWindow window
        loop
    handleEvents = do
      ev' <- pollEventWithImGui
      case ev' of
        Nothing -> return False
        Just ev
          | QuitEvent <- eventPayload ev -> return True
          | WindowResizedEvent r <- eventPayload ev -> do
            let V2 w h = windowResizedEventSize r
            glViewport 0 0 w h
            onEvent (eventPayload ev) appst
            handleEvents
          | otherwise -> do
            onEvent (eventPayload ev) appst
            handleEvents
