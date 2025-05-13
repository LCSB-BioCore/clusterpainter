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

main :: IO ()
main = do
  initializeAll
  runManaged $ do
    window <-
      do
        let title = "Hello, Dear ImGui!"
        let config =
              defaultWindow
                {windowGraphicsContext = OpenGLContext defaultOpenGL}
        managed $ bracket (createWindow title config) destroyWindow
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    _ <- managed $ bracket createContext destroyContext
    managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    managed_ $ bracket_ openGL3Init openGL3Shutdown
    liftIO $ mainLoop window

mainLoop :: Window -> IO ()
mainLoop window =
  unlessQuit $ do
  -- Tell ImGui we're starting a new frame
    openGL3NewFrame
    sdl2NewFrame
    newFrame
    withWindowOpen "Hello, ImGui!" mainWidget
    showDemoWindow
    glClear GL_COLOR_BUFFER_BIT
    render
    openGL3RenderDrawData =<< getDrawData
    glSwapWindow window
    mainLoop window
  where
  -- Process the event loop
    unlessQuit action = do
      shouldQuit <- gotQuitEvent
      unless shouldQuit action
    gotQuitEvent = do
      ev <- pollEventWithImGui
      case ev of
        Nothing -> return False
        Just event -> (isQuit event ||) <$> gotQuitEvent
    isQuit event = eventPayload event == QuitEvent

mainWidget :: IO ()
mainWidget = do
  text "Hello, ImGui!"
  button "clickety clicky" >>= \c -> when c $ putStrLn "Ow!"
