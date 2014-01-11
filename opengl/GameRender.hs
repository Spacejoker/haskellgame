module GameRender where

import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Graphics.Rendering.FTGL
import Graphics.UI.GLUT (elapsedTime, get)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Control.Monad
import System.Random
import Data.IORef
import Data.Bits ( (.|.) )

import Cube
import Model
import StringUtil
import GameTick
import RenderUtil
import System.Random
import Data.String.Utils

displayPlay gs gx =do
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  setup2D
  
  gs' <- readIORef gs 
  let target = targetStr gs'
      curStr' = curStr gs'
      common = commonStr gs'--commonPrefix curStr' target
  t <- get elapsedTime
  drawText (font gx) target (30, 30) (1,1,1)
  drawText (font gx) (writeFormat curStr') (30, 58) (1,0,0)
  drawText (font gx) (writeFormat common) (30, 86) (0,0.8,0)
  drawText (font gx) ("Score: " ++ (show $ score gs')) (30, 116) (0,0.8,0)
  drawText (font gx) ("Time: " ++ (show $ timeLeft gs' t)) (30, 144) (0,0.8,0)
  glFlush

timeLeft :: GameState -> Int -> Int
timeLeft gs t = ceiling ((fromIntegral turnLength) - ( (fromIntegral t) - (fromIntegral $ t0 gs))/1000)

writeFormat :: String -> String
writeFormat s = replace " " "_" s


