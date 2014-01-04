module Display where

import qualified Graphics.UI.GLFW as GLFW
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Control.Monad
import System.Random
import Data.IORef

import Cube
import Model
import StringUtil
import GameTick
import GameRender
import MenuRender
import GameOverRender

--display :: IORef GameState -> Graphics -> GLFW.Window  -> IO ()
--drawScene :: GLuint -> IORef GLfloat -> 
--display gs gx _ = do
display :: IORef GameState -> Graphics -> GLFW.Window  -> IO ()
display gs gx _  = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  glTranslatef 0 0 (-6.0) 

  glRotatef 50 1 0 0
  glRotatef 25 0 2 0

  cube 1 (Just $ texCube gx)
  --writeIORef angle  $! angle_ + 0.1
  --angle $~! \angle -> (angle + 0.1)

  glFlush
  --gs' <- readIORef gs
  --delegateRender (mode gs') gs gx
  

delegateRender :: GameMode -> IORef GameState -> Graphics-> IO()
delegateRender Play gs gx = renderGame gs
delegateRender Title gs gx = renderMenu gs gx
delegateRender GameOver gs gx = renderGameOver gs
