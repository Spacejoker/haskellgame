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

  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT

  setup3D

  glBindTexture gl_TEXTURE_2D (texCube gx)
 
  glRotatef 50 1 0 0
  glRotatef 25 0 2 0
  t <- get elapsedTime 
  let f = (\x -> (x, 2 + sin ((-x)+(fromIntegral t)/200.0)))
      fsin = map f [-5,-4..5]
  drawSinCubes fsin (Just $ texCube gx) (fromIntegral t)

  setup2D
  
  gs' <- readIORef gs 
  let mod = sin((fromIntegral t)/200)*0.3
      textColor = (0.2, 0.2, 0.3)
      chosenColor = (0.5+mod, 0.5+mod, 0.5+mod)
      xBase = 570
  renderTextMenu (font gx) 
               [("New Game", (xBase, 400)),( "Credits", (xBase + 12, 440)), ("  Quit", (xBase, 480))]
               textColor chosenColor 0 (menuChoice gs')
  glFlush

displayScene Play gs gx = do
  
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  setup2D
  
  gs' <- readIORef gs 
  let target = targetStr gs'
      curStr' = curStr gs'
      common = commonStr gs'--commonPrefix curStr' target
  drawText (font gx) target (30, 30) (1,1,1)
  drawText (font gx) (writeFormat curStr') (30, 58) (1,0,0)
  drawText (font gx) (writeFormat common) (30, 86) (0,0.8,0)
  drawText (font gx) ("Score: " ++ (show $ score gs')) (30, 116) (0,0.8,0)
  glFlush

writeFormat :: String -> String
writeFormat s = replace " " "_" s

renderTextMenu :: Font -> [(String, (GLfloat, GLfloat))]-> (GLfloat, GLfloat, GLfloat) ->
                (GLfloat, GLfloat, GLfloat) -> Int -> Int -> IO()
renderTextMenu _ [] _ _ _ _ = return ()
renderTextMenu fnt ((s, (x, y)):xs)  unchosen chosen cur choice
  | cur == choice = do
    drawText fnt s (x, y) chosen
    renderTextMenu fnt xs unchosen chosen (cur+1) choice
  | otherwise = do
    drawText fnt s (x, y) unchosen
    renderTextMenu fnt xs unchosen chosen (cur+1) choice
    

drawText :: Font -> String -> (GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> IO()
drawText fnt s (x, y) (r, g, b) = do
  glColor3f r g b
  glRasterPos2f x y
  setFontFaceSize fnt 24 72
  renderFont fnt s All

drawSinCubes :: [(GLfloat, GLfloat)] -> Maybe GLuint -> GLfloat -> IO()
drawSinCubes [] _ t = return ()
drawSinCubes ((a, b):xs) tex t = do
  glLoadIdentity  -- reset view
  glTranslatef a b (-6.0) 

  glRotatef (t*0.2) 1 0 0
  glRotatef (t*0.08) 0 2 0
  cube 0.2 tex
  drawSinCubes xs tex t
