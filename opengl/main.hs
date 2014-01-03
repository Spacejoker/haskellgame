import Graphics.UI.GLUT
import Model
import Bindings
import GameTick
import Data.IORef

main :: IO()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 640 480
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  pos <- newIORef (0, 0)
  units <- newIORef [(5,6), (4,5)]
  curStr <- newIORef ""
  let enemies = [Enemy (0,0) 10 10]
  gs <- newIORef $ GameState "Write me" "" 0 0 enemies False
  keyboardMouseCallback $= Just (keyboardMouse gs)
  idleCallback $= Just (idle gs)
  displayCallback $= display gs
  initMatrix
  mainLoop
