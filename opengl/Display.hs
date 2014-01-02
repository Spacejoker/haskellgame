module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Points
import Cube


vert :: Int -> Int -> GLfloat -> Vertex3 GLdouble
vert x y z = Vertex3 (fromIntegral x) (fromIntegral y) (realToFrac z)

display ::IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do

  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  let x' = 0::GLfloat
  let y' = 0::GLfloat
  let z' = -10::GLfloat
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    color (Color3 1.0 1.0 (1.0::GLfloat))
    cube 0.5
    a <- get angle
    rotate a $ Vector3 0 0.1 1
    rotate a $ Vector3 0 1 1
    forM_ (points 20) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 (x*0.7) (y*0.7) (-10)
      cube 0.02
      color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 0.02
  swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing
