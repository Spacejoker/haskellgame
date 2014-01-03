module GameOverRender where

import Graphics.UI.GLUT
import Data.IORef

import Model
import RenderUtil

renderGameOver :: IORef GameState -> DisplayCallback
renderGameOver gs = do

  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  gs' <- get gs
  drawString "GAME OVER" (Vector3 0 200 (0::GLfloat)) (Color3 1 1 (1::GLfloat))
  drawString ("Total score: " ++ (show $ score gs')) (Vector3 (-200) 100 (0::GLfloat)) (Color3 1 1 (1::GLfloat))
  drawString "Press space key to go back" (Vector3 (-500) 0 (0::GLfloat)) (Color3 1 1 (1::GLfloat))
  

  swapBuffers
  
