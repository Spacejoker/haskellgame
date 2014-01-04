module Bindings where

import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT as GLUT
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
