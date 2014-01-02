module Bindings (setCamera, idle, display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Display
import Data.IORef

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (*2)
  (Char '-') -> a $~! (/ 2)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()


