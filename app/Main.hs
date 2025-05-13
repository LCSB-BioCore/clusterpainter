{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket, bracket_)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class ()
import Data.IORef
import qualified Data.Set as S
import qualified Data.Vector as V
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Rendering
import SDL

import St
import Config

renderApp xs ys appst = pure $ Circle 30

onEvent (WindowResizedEvent r) _ =
  let V2 w h = windowResizedEventSize r
   in glViewport 0 0 w h
onEvent _ _ = pure ()

drawUI appst = do
  showDemoWindow
  withWindowOpen "Data input" $ do
    text "Hello, ImGui!"
    button "clickety clicky" >>= \c -> when c $ putStrLn "Ow!"
    --void $ sliderInt "haha" (appst :: IORef Int) 0 100
  {-
  withWindowOpen "Display" $ do
    pure ()
  withWindowOpen "Clustering" $ do
    pure ()
  -}

--TODO make a custom wrap for StateVar that has IORef in it, produce "zoom" etc. Multiple IORefs might need to be produced for each level of the structure.
----------
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
        renderApp wsx wsy appst
          >>= displayPicture (wsx, wsy) (greyN 0.25) st 1.0
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
          | eventPayload ev == QuitEvent -> return True
          | otherwise -> onEvent (eventPayload ev) appst >> return False
