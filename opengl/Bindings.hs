module Bindings (setCamera, idle, display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Display
import Data.IORef

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

keyboardMouse :: IORef String -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse s a p key Down _ _ = case key of
  --(Char ' ') -> a $~! negate
  (Char '\b') -> s $~! \(x:xs) -> xs--writeIORef s (x:s')
  (Char '+') -> a $~! (*2)
  (Char '-') -> a $~! (/ 2)
  (Char x) -> s $~! \str -> (x:str)--writeIORef s (x:s')
  _ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()


