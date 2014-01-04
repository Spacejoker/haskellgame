module Display where

import Graphics.UI.GLUT
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

display :: IORef GameState -> Graphics -> DisplayCallback
display gs gx = do
  gs' <- get gs
  delegateRender (mode gs') gs gx

delegateRender :: GameMode -> IORef GameState -> Graphics-> DisplayCallback
delegateRender Play gs gx = renderGame gs
delegateRender Title gs gx = renderMenu gs gx
delegateRender GameOver gs gx = renderGameOver gs

initMatrix = do
  viewport $= (Position 0 0,Size 1280 720)
  matrixMode $= Projection

  loadIdentity 
  perspective 30.0 (16/9) 1 140000
  setCamera 0 0

setCamera xtarget ztarget = do
  lookAt (Vertex3 0 0 (50::Double)) (Vertex3 0 0 (0::Double)) (Vector3 0 1 (0::Double))
