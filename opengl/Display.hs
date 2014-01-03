module Display where

import Graphics.UI.GLUT
import Control.Monad
import System.Random
import Data.IORef

import Points
import Cube
import Model
import StringUtil
import GameTick

initMatrix = do
  viewport $= (Position 0 0,Size 640 480)
  matrixMode $= Projection

  loadIdentity 
  perspective 30.0 (4/3) 1 140000
  setCamera 8 8

setCamera xtarget ztarget = do
  lookAt (Vertex3 0 10 (50::Double)) (Vertex3 0 0 (0::Double)) (Vector3 0 1 (0::Double))

drawString :: GameState -> IO()
drawString gs = do

  let pref = commonPrefix (reverse $ curStr gs) (targetStr gs)
      sc = (0.01::GLfloat)
  color (Color3 1 1 (0::GLfloat))


  scale sc sc sc
  translate $ Vector3 (-100) 250 (-100::GLfloat)
  renderString MonoRoman $ targetStr gs

  loadIdentity
  scale sc sc sc
  translate $ Vector3 (-100) 450 (-100::GLfloat)
  renderString MonoRoman $ reverse $ curStr gs

  loadIdentity
  scale sc sc sc
  translate $ Vector3 (100) 800 (-100::GLfloat)
  renderString MonoRoman $ show $ score gs

renderMenu :: IORef GameState -> DisplayCallback
renderMenu iogs = do
  putStrLn "APA" 
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  let width = 10 
      boxes = [((x::GLfloat), 0, z) | x <- [0..width], z <- [0..width]]

  preservingMatrix $ do
    color (Color3 1.0 1.0 (1.0::GLfloat))
    forM_ boxes $ \(x, y, z) -> preservingMatrix $ do
      loadIdentity
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 (x*2) 0 (z*2)
      cube 1
      color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 1
  swapBuffers

delegateRender :: Bool -> IORef GameState -> DisplayCallback
delegateRender False gs = renderGame gs
delegateRender _ gs = renderMenu gs

display :: IORef GameState -> DisplayCallback
display str = do
  gs <- get str
  delegateRender (gameOver gs) str

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
    drawString gs
    color (Color3 1 0 (0::GLfloat))
    scale 1 1 (1::GLfloat)

  preservingMatrix $ do
    color (Color3 1.0 1.0 (1.0::GLfloat))
    forM_ boxes $ \(x, y, z) -> preservingMatrix $ do
      loadIdentity
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      --translate $ Vector3 (x*2) 0 (z*2)
      --cube 1
      --color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 1

  -- draw enemies
  preservingMatrix $ do
    color (Color3 0 1 (1::GLfloat))
    forM_ (map enemyPos $ enemies gs) $ \(x, z) -> preservingMatrix $ do
      loadIdentity
      translate $ Vector3 (x) 0.0 (z*2.0::GLfloat)
      cube 1
      color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 1

  swapBuffers


