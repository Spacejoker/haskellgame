module MenuRender where

import Graphics.UI.GLUT as GLUT -- (DisplayCallback, ($=), ortho2D, swapBuffers)
--ssimport Graphics.Rendering.OpenGL.GL as GL --(textureBinding, ($=), Texture2D)
import Control.Monad
import System.Random
import Data.IORef

import Cube
import Model
import StringUtil
import GameTick
import RenderUtil

renderMenu :: IORef GameState -> Graphics -> DisplayCallback
renderMenu iogs gx = do

  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  textureBinding Texture2D $= Just (texCube gx)

  let width = 10 
      boxes = [((x::GLfloat), 0, z) | x <- [0..width], z <- [1..width]]

  preservingMatrix $ do
    color (Color3 1.0 1.0 (1.0::GLfloat))
    forM_ boxes $ \(x, y, z) -> preservingMatrix $ do
      loadIdentity
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 (x*2) 0 (z*2)
      --cube 1
      color $ Color3 (0 ::GLfloat) 0 0 
      --cubeFrame 1

  --drawString "New Game: 'n'" (Vector3 0 200 (0::GLfloat)) (Color3 1 1 (1::GLfloat))
  ortho2D 0 1024 0 720 --(-1000) (1000::GLdouble)
  loadIdentity 
  color (Color3 1.0 1.0 (1.0::GLfloat))
  --rectangle (-10) 0 5 5
  --drawString "IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII" (Vector3 (-2000) 400 (0::GLfloat)) (Color3 1 1 (1::GLfloat))
  
  swapBuffers

rectangle :: Double -> Double -> Double -> Double -> IO ()
rectangle x y w h = renderPrimitive Quads $ mapM_ (vertex . vert2D)
                        [(x, y), (x + w , y ), (x + w, y - h ), (x , y - h )]

