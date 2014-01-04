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

  swapBuffers
