module Display (idle, display, setCamera) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Points
import Cube


vert :: Int -> Int -> GLfloat -> Vertex3 GLdouble
vert x y z = Vertex3 (fromIntegral x) (fromIntegral y) (realToFrac z)

setCamera xtarget ztarget = do
  --matrixMode $= Projection
  --loadIdentity
  lookAt (Vertex3 (10+xtarget) 10 ((10+ztarget)::Double)) (Vertex3 xtarget 0 (ztarget::Double)) (Vector3 0 1 (0::Double))

display :: IORef String -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef [(GLfloat, GLfloat)] -> DisplayCallback
display str angle pos units = do
  --curStr <- get str
  --putStrLn curStr  
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  
  let width = 10 
      boxes = [((x::GLfloat), 0, z) | x <- [0..width], z <- [0..width]]

  preservingMatrix $ do
    color (Color3 1 0 (0::GLfloat))
    scale 0.001 0.001 (0.001::GLfloat)
    renderString Roman "Test string"
    scale 1 1 (1::GLfloat)
    units' <- get units
    forM_ units' $ \(x, z) -> preservingMatrix $ do
      loadIdentity
      translate $ Vector3 (x*2) 2 (z*2)
      color (Color3 1 0 (0::GLfloat))
      cube 0.8
      color (Color3 0 0 (0::GLfloat))
      cubeFrame 0.8

  preservingMatrix $ do
    color (Color3 1.0 1.0 (1.0::GLfloat))
    forM_ boxes $ \(x, y, z) -> preservingMatrix $ do
      loadIdentity
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 (x*2) 0 (z*2)
      cube 1
      color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 1
    cube 1
    cubeFrame 1
  swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing

