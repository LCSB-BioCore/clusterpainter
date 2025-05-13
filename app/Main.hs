{-# LANGUAGE OverloadedStrings #-}

import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL

import Control.Exception (bracket, bracket_)
import Control.Monad (unless, when)
import Control.Monad.IO.Class ()
import Control.Monad.Managed

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Rendering

main :: IO ()
main = do
  initializeAll
  runManaged $ do
    window <-
      do
        let title = "Hello, Dear ImGui!"
        let config =
              defaultWindow
                { windowGraphicsContext = OpenGLContext defaultOpenGL
                , windowResizable = True
                }
        managed $ bracket (createWindow title config) destroyWindow
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    _ <- managed $ bracket createContext destroyContext
    managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    managed_ $ bracket_ openGL3Init openGL3Shutdown
    st <- liftIO $ initState
    liftIO $ mainLoop st window

mainLoop :: State -> Window -> IO ()
mainLoop st window = do
  finished <- handleEvents
  unless finished $ do
    -- start frame
    openGL3NewFrame
    sdl2NewFrame
    newFrame
    -- rendering
    (V2 wsx wsy) <- fmap fromIntegral <$> get (windowSize window)
    displayPicture (wsx, wsy) (greyN 0.25) st 1.0 (Circle 30)
    -- UI
    withWindowOpen "Hello, ImGui!" mainWidget
    showDemoWindow
    -- UI rendering
    render
    openGL3RenderDrawData =<< getDrawData
    -- post-frame
    glSwapWindow window
    mainLoop st window
  where
    handleEvents = do
      ev' <- pollEventWithImGui
      case ev' of
        Nothing -> return False
        Just ev
          | eventPayload ev == QuitEvent -> return True
          | otherwise -> onEvent (eventPayload ev) >> return False
    onEvent (WindowResizedEvent r) =
      let V2 w h = windowResizedEventSize r
       in glViewport 0 0 w h
    onEvent _ = pure ()

mainWidget :: IO ()
mainWidget = do
  text "Hello, ImGui!"
  button "clickety clicky" >>= \c -> when c $ putStrLn "Ow!"
