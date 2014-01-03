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

display :: IORef GameState -> DisplayCallback
display str = do
  gs <- get str
  putStrLn $ "OK: " ++ (show $ (mode gs))
  delegateRender (mode gs) str

delegateRender :: GameMode -> IORef GameState -> DisplayCallback
delegateRender Play gs = renderGame gs
delegateRender Title gs = renderMenu gs
delegateRender GameOver gs = renderGameOver gs

initMatrix = do
  viewport $= (Position 0 0,Size 640 480)
  matrixMode $= Projection

  loadIdentity 
  perspective 30.0 (4/3) 1 140000
  setCamera 8 8

setCamera xtarget ztarget = do
  lookAt (Vertex3 0 10 (50::Double)) (Vertex3 0 0 (0::Double)) (Vector3 0 1 (0::Double))
