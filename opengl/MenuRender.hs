module MenuRender where

import Graphics.UI.GLUT
import Control.Monad
import System.Random
import Data.IORef

import Cube
import Model
import StringUtil
import GameTick
import RenderUtil

renderMenu :: IORef GameState -> DisplayCallback
renderMenu iogs = do

  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  let width = 10 
      boxes = [((x::GLfloat), 0, z) | x <- [0..width], z <- [1..width]]

  preservingMatrix $ do
    color (Color3 1.0 1.0 (1.0::GLfloat))
    forM_ boxes $ \(x, y, z) -> preservingMatrix $ do
      loadIdentity
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 (x*2) 0 (z*2)
      cube 1
      color $ Color3 (0 ::GLfloat) 0 0 
      cubeFrame 1



  --matrixMode $= Projection

  --perspective 30.0 (16/9) 1 140000
  drawString "New Game: 'n'" (Vector3 0 200 (0::GLfloat)) (Color3 1 1 (1::GLfloat))
  ortho2D 0 1024 0 720 --(-1000) (1000::GLdouble)
  loadIdentity 
  color (Color3 1.0 1.0 (1.0::GLfloat))
  rectangle 0 0 20 20
  drawString "IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII" (Vector3 (-2000) 400 (0::GLfloat)) (Color3 1 1 (1::GLfloat))
  --setCamera 8 8
--glLoadIdentity ();
--gluOrtho2D (0, windowWidth, 0, windowHeight);
  
  swapBuffers

rectangle :: Double -> Double -> Double -> Double -> IO ()
rectangle x y w h = renderPrimitive Quads $ mapM_ (vertex . vert2D)
                        [(x - w / 2, y - h / 2), (x + w / 2, y - h / 2),
                         (x + w / 2, y + h / 2), (x - w / 2, y + h / 2)]

