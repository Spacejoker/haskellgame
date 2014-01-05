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
  writeIORef gs $! gs' { mode = modeFromInt $ menuChoice gs' }
handleEvent gs Title GLFW.Key'Up = do
  gs' <- readIORef gs
  writeIORef gs $! gs' {menuChoice = max ((menuChoice gs') - 1) 0}
handleEvent gs Title GLFW.Key'Down = do
  gs' <- readIORef gs
  writeIORef gs $! gs' {menuChoice = min ((menuChoice gs') + 1) 2}

handleEvent gs Credits GLFW.Key'Enter = do
  gs' <- readIORef gs
  writeIORef gs $! gs' { mode = Title }

handleEvent gs Play GLFW.Key'Backspace = do
  gs' <- readIORef gs
  --putStrLn $ show key
  let (x:xs) = curStr gs'
  writeIORef gs $! gs' { curStr = xs }

handleEvent _  _       _ = return ()

textInput :: IORef GameState -> GLFW.CharCallback
textInput gs a b = do
  gs' <- readIORef gs
  appendWriting (mode gs') b gs

appendWriting :: GameMode -> Char -> IORef GameState -> IO ()
appendWriting Play c gs = do
  gs' <- readIORef gs
  let curStr' = curStr gs'
  writeIORef gs $! gs' { curStr = (c:curStr') }
  putStrLn $ curStr'
appendWriting _ _ _ = return ()

-- OLD SHIT
reshape :: ReshapeCallback
reshape size = do
  GLUT.viewport GLUT.$= (GLUT.Position 0 0, size)
  postRedisplay Nothing
keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse gs key Down _ _ = do
  gs' <- get gs
  handleInput (mode gs') gs key
keyboardMouse _ _ _ _ _ = return ()

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
