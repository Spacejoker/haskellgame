module Bindings where

import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw
import System.Exit ( exitWith, ExitCode(..) )
import Display
import Data.IORef
import Model

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: IORef GameState -> GLFW.KeyCallback 
keyPressed gs win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed gs _ key _ GLFW.KeyState'Pressed _ = do
  gs' <- readIORef gs
  let mode' = mode gs'
  handleEvent gs mode' key
keyPressed _  _   _               _ _                     _ = return ()

handleEvent :: IORef GameState -> GameMode -> GLFW.Key -> IO()
handleEvent gs Title GLFW.Key'Enter = do
  gs' <- readIORef gs
  t0' <- get elapsedTime
  
  writeIORef gs $! case (modeFromInt $ menuChoice gs') of {
    (Play) -> gs' { mode = Play, t0 = t0' };
    (Credits) -> gs'
  }


handleEvent gs Title GLFW.Key'Up = do
  gs' <- readIORef gs
  writeIORef gs $! gs' {menuChoice = max ((menuChoice gs') - 1) 0}

handleEvent gs Title GLFW.Key'Down = do
  gs' <- readIORef gs
  writeIORef gs $! gs' {menuChoice = min ((menuChoice gs') + 1) 2}

handleEvent gs Credits GLFW.Key'Enter = do
  gs' <- readIORef gs
  writeIORef gs $! gs' { mode = Title }

handleEvent gs GameOver GLFW.Key'Enter = do
  gs' <- readIORef gs
  writeIORef gs $! gs' { mode = Title }

handleEvent gs Play GLFW.Key'Backspace = do
  gs' <- readIORef gs
  writeIORef gs $! gs' { curStr = "" } --Prelude.init $ curStr gs' }

handleEvent _  _       _ = return ()

textInput :: IORef GameState -> GLFW.CharCallback
textInput gs a b = do
  gs' <- readIORef gs
  appendWriting (mode gs') b gs

appendWriting :: GameMode -> Char -> IORef GameState -> IO ()
appendWriting Play c gs = do
  gs' <- readIORef gs
  let curStr' = curStr gs'
  writeIORef gs $! gs' { curStr = curStr' ++ [c] }

appendWriting _ _ _ = return ()

