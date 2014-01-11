module Display where

import Graphics.UI.GLUT (elapsedTime, get)
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Graphics.Rendering.FTGL

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
import RenderUtil
import Data.String.Utils


display :: IORef GameState -> Graphics -> GLFW.Window  -> IO ()
display gs gx _  = do
  gs' <- readIORef gs
  displayScene (mode gs') gs gx 
  

displayScene :: GameMode -> IORef GameState -> Graphics -> IO ()
displayScene Credits gs gx  = do

  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  setup2D
  drawText (font gx) "Super game created by Jens Schwarzenegger and friends" (300, 200) (1,1,0)

  glFlush

displayScene Title gs gx  = do
  displayTitle gs gx

displayScene Play gs gx = do
  displayPlay gs gx 

displayScene GameOver gs gx = do
  
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  setup2D
  drawText (font gx) "Some game over info, enter to continue" (300, 200) (1,1,0)

  glFlush
