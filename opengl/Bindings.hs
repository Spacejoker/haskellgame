module Bindings where

import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw
import System.Exit ( exitWith, ExitCode(..) )
import Display
import Data.IORef
import Model

reshape :: ReshapeCallback
reshape size = do
  GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)
  postRedisplay Nothing

keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse gs key Down _ _ = do
  gs' <- get gs
  handleInput (mode gs') gs key
keyboardMouse _ _ _ _ _ = return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: IORef GameState -> GLFW.KeyCallback 
keyPressed gs win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed gs win GLFW.Key'Enter _ GLFW.KeyState'Pressed _ = do
  gs' <- readIORef gs
  writeIORef gs $! gs' {mode = Play }
keyPressed gs win GLFW.Key'Up _ GLFW.KeyState'Pressed _ = do
  gs' <- readIORef gs
  writeIORef gs $! gs' {menuChoice = max ((menuChoice gs') - 1) 0}
keyPressed gs win GLFW.Key'Down _ GLFW.KeyState'Pressed _ = do
  gs' <- readIORef gs
  writeIORef gs $! gs' {menuChoice = min ((menuChoice gs') + 1) 2}
keyPressed _  _   _               _ _                     _ = return ()

handleInput Play gs key = case key of 
  (Char ';') -> gs $~! \gs -> gs{mode = GameOver}
  (Char '\b') -> gs $~! \gs -> gs{curStr = newStr}
    where newStr = ""
  (Char x) -> gs $~! \gs -> gs{curStr = (x:(curStr gs))}--writeIORef s (x:s')
  _ -> return ()

handleInput Title gs key = case key of
  (Char 'n') -> gs $~! \gs -> newGame { mode = Play }
  _ -> return ()

handleInput GameOver gs key = case key of
  (Char ' ') -> gs $~! \gs -> gs { mode = Title }   
  _ -> return ()

handleInput _ _ _ = return ()
