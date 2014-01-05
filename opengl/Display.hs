module Display where

import Graphics.UI.GLUT (elapsedTime, get)

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

display :: IORef GameState -> Graphics -> GLFW.Window  -> IO ()
display gs gx _  = do

  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT

  glLoadIdentity  -- reset view
  glTranslatef 0 0 (-6.0) 

  glRotatef 50 1 0 0
  glRotatef 25 0 2 0
  t <- get elapsedTime 
  let f = (\x -> (x, sin ((-x)+(fromIntegral t)/200.0)))
      fsin = map f [-5, -4..5]
  --map (\(x, y) -> putStrLn $ show x ) pos
  drawSinCubes fsin (Just $ texCube gx) (fromIntegral t)
  --cube 0.2 (Just $ texCube gx)

  glFlush

drawSinCubes :: [(GLfloat, GLfloat)] -> Maybe GLuint -> GLfloat -> IO()
drawSinCubes [] _ t = return ()
drawSinCubes ((a, b):xs) tex t = do
  glLoadIdentity  -- reset view
  glTranslatef a b (-6.0) 

  glRotatef (t*0.5) 1 0 0
  glRotatef (t*0.2) 0 2 0
  cube 0.2 tex
  drawSinCubes xs tex t

delegateRender :: GameMode -> IORef GameState -> Graphics-> IO()
delegateRender Play gs gx = renderGame gs
delegateRender Title gs gx = renderMenu gs gx
delegateRender GameOver gs gx = renderGameOver gs
