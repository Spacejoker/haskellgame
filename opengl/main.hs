import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Foreign ( withForeignPtr, plusPtr, peek, alloca )
import qualified Data.ByteString.Internal as BSI
import Data.Maybe
import Graphics.UI.GLUT (getArgsAndInitialize)

import Util
import Model
import Bindings
import GameTick
import Data.IORef
import Display
import Control.Monad
import Cube

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  True <- GLFW.init
  --loadImage
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 1280 720 "YOLO" Nothing Nothing
  GLFW.makeContextCurrent (Just win)

  font <- createBitmapFont "font.tff"
  putStrLn $ show font
  gs <- newIORef $ newGame 
  tex <- initGL win
  let gx = Graphics tex font
  
  --let f = CreateFont

  GLFW.setWindowRefreshCallback win (Just (display gs gx))

  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setKeyCallback win (Just $ keyPressed gs)
  GLFW.setWindowCloseCallback win (Just shutdown)

  forever $ do
    GLFW.pollEvents
    display gs gx win
    GLFW.swapBuffers win

initGL :: GLFW.Window -> IO GLuint
initGL win = do
  glEnable gl_TEXTURE_2D
  glShadeModel gl_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LEQUAL -- type of depth test
  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  loadGLTextures

loadGLTextures :: IO GLuint
loadGLTextures = do
  fp <- getDataFileName "tex.bmp"
  putStrLn $ "loading texture: " ++ fp
  Just (Image w h pd) <- bitmapLoad fp
  putStrLn $ "Image width  = " ++ show w
  putStrLn $ "Image height = " ++ show h
  tex <- alloca $ \p -> do
    glGenTextures 1 p
    peek p
  let (ptr, off, _) = BSI.toForeignPtr pd
  withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
    glBindTexture gl_TEXTURE_2D tex
    glTexImage2D gl_TEXTURE_2D 0 3
      (fromIntegral w) (fromIntegral h) 0 gl_RGB gl_UNSIGNED_BYTE
      p'
    let glLinear = fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glLinear
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glLinear
  return tex

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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  return ("/home/js/dev/haskell/rpg/test-texture/textures/" ++ name)

drawScene :: GLuint -> IORef GLfloat -> GLFW.Window  -> IO ()
drawScene tex angle _  = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  glTranslatef 0 0 (-6.0) 

  glRotatef 50 1 0 0
  glRotatef 25 0 2 0

  cube 1 (Just tex)
  --writeIORef angle  $! angle_ + 0.1
  --angle $~! \angle -> (angle + 0.1)

  glFlush


--main :: IO()
--main = do
--  (_progName, _args) <- getArgsAndInitialize
--  initialDisplayMode GLUT.$= [WithDepthBuffer, DoubleBuffered]
--  initialWindowSize GLUT.$= GLUT.Size 1280 720
--  _window <- createWindow "Hello World"
--  reshapeCallback GLUT.$= Just reshape
--  GLUT.depthFunc $= Just GLUT.Less
--  glEnable gl_TEXTURE_2D
--
--  pos <- newIORef (0, 0)
--  units <- newIORef [(5,6), (4,5)]
--  curStr <- newIORef ""
--  texCube  <- loadTexture "cube.tga"
--  let gx = Graphics texCube
--  gs <- newIORef $ newGame 
--  keyboardMouseCallback GLUT.$= Just (keyboardMouse gs)
--  idleCallback GLUT.$= Just (idle gs)
--  displayCallback GLUT.$= display gs gx
--  initMatrix
--  mainLoop
