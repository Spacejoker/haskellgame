module Display (idle, display, setCamera) where

import Graphics.UI.GLUT
import Control.Monad
import System.Random
import Data.IORef

import Points
import Cube
import Model
import StringUtil

vert :: Int -> Int -> GLfloat -> Vertex3 GLdouble
vert x y z = Vertex3 (fromIntegral x) (fromIntegral y) (realToFrac z)

setCamera xtarget ztarget = do
  --matrixMode $= Projection
  --loadIdentity
  lookAt (Vertex3 0 10 (50::Double)) (Vertex3 0 0 (0::Double)) (Vector3 0 1 (0::Double))

drawString :: GameState -> IO()
drawString gs = do
  let pref = commonPrefix (reverse $ curStr gs) (targetStr gs)
  --putStrLn pref
  color (Color3 1 1 (0::GLfloat))
  let sc = (0.01::GLfloat)
  scale sc sc sc
  translate $ Vector3 (-100) 250 (-100::GLfloat)
  renderString MonoRoman $ targetStr gs

  loadIdentity
  scale sc sc sc
  translate $ Vector3 (-100) 450 (-100::GLfloat)
  renderString MonoRoman $ reverse $ curStr gs

  loadIdentity
  scale sc sc sc
  translate $ Vector3 (100) 450 (-100::GLfloat)
  renderString MonoRoman $ show $ points gs

display :: IORef GameState  -> IORef [(GLfloat, GLfloat)] -> DisplayCallback
display str units = do

  gs <- get str
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
    units' <- get units
    forM_ units' $ \(x, z) -> preservingMatrix $ do
      loadIdentity
      translate $ Vector3 (x*2) 2 (z*2)
      color (Color3 1 0 (0::GLfloat))
      --cube 0.8
      --color (Color3 0 0 (0::GLfloat))
      cubeFrame 0.8

  preservingMatrix $ do
    color (Color3 1.0 1.0 (1.0::GLfloat))
    forM_ boxes $ \(x, y, z) -> preservingMatrix $ do
      loadIdentity
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      --translate $ Vector3 (x*2) 0 (z*2)
      --cube 1
      --color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 1
  swapBuffers

checkWord :: IORef GameState -> String -> String -> String -> IO()
checkWord gs tStr cStr pStr
  | length cStr > length pStr = gs $~! \gs -> gs{targetStr = "FAIL", curStr = ""}
  | length tStr == length pStr = do
    word' <- getWord
    gs $~! \gs -> gs{targetStr = word', curStr = ""}
  | otherwise = return ()

idle :: IORef GameState -> IdleCallback
idle gs = do

  gs' <- get gs
  let tStr = targetStr gs'
      cStr = reverse $ curStr gs'
      pStr = commonPrefix (targetStr gs') (reverse $ curStr gs')
  checkWord gs tStr cStr pStr

  rng <- getStdRandom (randomR (1,(6::Int)))
  postRedisplay Nothing

