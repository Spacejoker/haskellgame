import Graphics.UI.GLUT
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
  angle <- newIORef 0.0
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  idleCallback $= Just (idle angle delta)
  displayCallback $= display angle pos
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
  lookAt (Vertex3 0 0 (30::Double)) (Vertex3 0 0 (0::Double)) (Vector3 0 1 (0::Double))
