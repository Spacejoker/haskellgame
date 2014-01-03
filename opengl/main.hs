import Graphics.UI.GLUT
import Model
import Bindings
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
  gs <- newIORef $ GameState "Write me" ""
  keyboardMouseCallback $= Just (keyboardMouse gs)
  idleCallback $= Just (idle)
  displayCallback $= display gs units
  initMatrix
  mainLoop

initMatrix = do
  --return ()
  viewport $= (Position 0 0,Size 640 480)
  matrixMode $= Projection
--  loadIdentity
--  ortho (-320) 320 (-240) 240 (-1000) 1000        
  loadIdentity 
  perspective 30.0 (4/3) 1 140000
  setCamera 8 8

