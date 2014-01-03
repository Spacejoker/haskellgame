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

drawStrings :: GameState -> IO()
drawStrings gs = do

  let sc = (0.01::GLfloat)
      color' = (Color3 1 1 (0::GLfloat))

  drawString (targetStr gs) (Vector3 (-100) 250 (-100::GLfloat)) color'
  drawString (reverse $ curStr gs) ( Vector3 (-100) 450 (-100::GLfloat) ) color'
  drawString (show $ score gs) ( Vector3 (100) 800 (-100::GLfloat) ) color'

renderGame :: IORef GameState -> DisplayCallback
renderGame iogs = do
  gs <- get iogs
  let curStr' = curStr gs
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  
  let width = 10 
      boxes = [((x::GLfloat), 0, z) | x <- [0..width], z <- [0..width]]

  preservingMatrix $ do
    drawStrings gs
    color (Color3 1 0 (0::GLfloat))
    scale 1 1 (1::GLfloat)

  preservingMatrix $ do
    color (Color3 0 1 (1::GLfloat))
    forM_ (map enemyPos $ enemies gs) $ \(x, z) -> preservingMatrix $ do
      loadIdentity
      translate $ Vector3 (x) 0.0 (z*2.0::GLfloat)
      cube 1
      color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 1

  swapBuffers
