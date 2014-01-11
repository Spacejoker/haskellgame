module GameRender where

import Graphics.UI.GLUT
import Control.Monad
import System.Random
import Data.IORef

import Cube
import Model
import StringUtil
import GameTick
import RenderUtil
import System.Random
displayPlay gs gx =do
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

drawStrings :: GameState -> IO()
drawStrings gs = do

  let sc = (0.01::GLfloat)
      color' = (Color3 1 1 (0::GLfloat))

  --drawString (targetStr gs) (Vector3 (-100) 250 (-100::GLfloat)) color'
  --drawString (reverse $ curStr gs) ( Vector3 (-100) 450 (-100::GLfloat) ) color'
  --drawString (show $ score gs) ( Vector3 (100) 800 (-100::GLfloat) ) color'
  return ()

renderGame :: IORef GameState -> DisplayCallback
renderGame iogs = do

  swapBuffers
