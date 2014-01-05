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

display :: IORef GameState -> Graphics -> GLFW.Window  -> IO ()
display gs gx _  = do

  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  let width = 1280
      height = 720
  glEnable gl_TEXTURE_2D
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (16.0/9.0) 0.1 100 
  glMatrixMode gl_MODELVIEW
  glEnable gl_DEPTH_TEST
  glLoadIdentity

  glBindTexture gl_TEXTURE_2D (texCube gx)
  glColor3f 1 1 1

  glRotatef 50 1 0 0
  glRotatef 25 0 2 0
  t <- get elapsedTime 
  let f = (\x -> (x, sin ((-x)+(fromIntegral t)/200.0)))
      fsin = map f [-5, -4..5]
  --map (\(x, y) -> putStrLn $ show x ) pos
  drawSinCubes fsin (Just $ texCube gx) (fromIntegral t)
  --cube 0.2 (Just $ texCube gx)

  glMatrixMode gl_PROJECTION
  glDisable gl_TEXTURE_2D
  glLoadIdentity
  glOrtho 0.0 width height 0.0 (-1.0) 1.0
  glMatrixMode gl_MODELVIEW
  glDisable gl_DEPTH_TEST
  glLoadIdentity
  --glLoadIdentity  -- reset view
  glColor3f 1 0.2 0.2
  ---putStrLn $ show $ font gx
  glRasterPos2f 10 20
  setFontFaceSize (font gx) 24 72
  --glTranslated 100 100 0
  renderFont (font gx) "Super awesome openGL text - nemas problemas" All

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
