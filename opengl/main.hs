import Graphics.UI.GLUT as GLUT
import Model
import Bindings
import GameTick
import Data.IORef
import Display

--import Graphics.Rendering.OpenGL.GL as GL
--import Graphics.Rendering.OpenGL.GLU as GLU
--import Graphics.UI.GLFW as GLFW
--import Graphics.Rendering.OpenGL (($=))
import Control.Monad

main :: IO()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode GLUT.$= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize GLUT.$= GLUT.Size 1280 720
  _window <- createWindow "Hello World"
  reshapeCallback GLUT.$= Just reshape
  GLUT.depthFunc $= Just GLUT.Less
  glEnable gl_TEXTURE_2D

  pos <- newIORef (0, 0)
  units <- newIORef [(5,6), (4,5)]
  curStr <- newIORef ""
  texCube  <- loadTexture "cube.tga"
  let gx = Graphics texCube
  gs <- newIORef $ newGame 
  keyboardMouseCallback GLUT.$= Just (keyboardMouse gs)
  idleCallback GLUT.$= Just (idle gs)
  displayCallback GLUT.$= display gs gx
  initMatrix
  mainLoop


