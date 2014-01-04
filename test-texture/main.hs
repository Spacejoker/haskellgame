--
-- This code was created by Jeff Molofee '99 (ported to Haskell GHC 2005)
--

module Main where

-- import Graphics.UI.GLUT
import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

initGL :: GLFW.Window -> IO ()
initGL win = do
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.WindowSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100 
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: IORef GLfloat -> GLFW.Window  -> IO ()
drawScene angle _  = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  glTranslatef 0 0 (-6.0) --Move left 1.5 Units and into the screen 6.0

  angle_ <- readIORef angle
  --putStrLn angle
  glRotatef angle_ 0 0 1
  glBegin gl_TRIANGLES
  glColor3f 1 0 0
  glVertex3f 0 1 0
  glColor3f 1 0 1
  glVertex3f (-1) (-1) 0
  glColor3f 1 1 0
  glVertex3f 1 (-1) 0
  glEnd

  glLoadIdentity
  glTranslatef 0 0 (-5.0) --Move left 1.5 Units and into the screen 6.0

  glRotatef (angle_*20) 1 0.3 0.5
  glRotatef (angle_) 0 1 0
  -- cube
  let w = 1
  glBegin gl_QUADS

  glColor3f 1 0.5 0

  glVertex3f 0 0 0
  glVertex3f w 0 0
  glVertex3f w w 0
  glVertex3f 0 w 0

  glColor3f 1 0 0

  glVertex3f 0 0 0
  glVertex3f 0 0 w
  glVertex3f 0 w w
  glVertex3f 0 w 0

  glColor3f 0 0 1

  glVertex3f w 0 0
  glVertex3f w 0 w
  glVertex3f w w w
  glVertex3f w w 0

  glColor3f 0 0.5 0

  glVertex3f 0 0 w
  glVertex3f w 0 w
  glVertex3f w w w
  glVertex3f 0 w w

  glColor3f 0 0.5 1

  glVertex3f 0 0 0
  glVertex3f w 0 0
  glVertex3f w 0 w
  glVertex3f 0 0 w

  glColor3f 1 0.5 1

  glVertex3f 0 w 0
  glVertex3f w w 0
  glVertex3f w w w
  glVertex3f 0 w w

  glEnd

  

  writeIORef angle  $! angle_ + 0.1
  --angle $~! \angle -> (angle + 0.1)

  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

main :: IO ()
main = do
     True <- GLFW.init
     GLFW.defaultWindowHints
     -- get a 640 x 480 window
     -- initialize our window.
     Just win <- GLFW.createWindow 1280 720 "Lesson 1" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     angle <- newIORef 0
     GLFW.setWindowRefreshCallback win (Just (drawScene angle))
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just keyPressed)
     -- register window close handler
     GLFW.setWindowCloseCallback win (Just shutdown)
     initGL win
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene angle win
       GLFW.swapBuffers win
