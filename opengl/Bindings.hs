module Bindings (initMatrix, setCamera, display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Display
import Data.IORef
import Model

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse gs key Down _ _ = case key of
  (Char '\b') -> gs $~! \gs -> gs{curStr = newStr}
    where newStr = ""
  (Char x) -> gs $~! \gs -> gs{curStr = (x:(curStr gs))}--writeIORef s (x:s')
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()


