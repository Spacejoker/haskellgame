module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Points
import Cube

points' :: [(GLfloat,GLfloat,GLfloat)]
points' = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

display ::IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  (x', y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0.1 1
    rotate a $ Vector3 0 1 1
    forM_ (points 7) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 (x*0.7) (y*0.7) z
      cube 0.1
      color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 0.1
  swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing
