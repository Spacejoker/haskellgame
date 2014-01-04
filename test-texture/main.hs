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
import Foreign ( withForeignPtr, plusPtr, peek, alloca )
import qualified Data.ByteString.Internal as BSI
import Data.Maybe
--import Codec.Picture
--import Paths_nehe_tuts

import Util
--import Cube


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
  --return (1::GLuint)
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

cube :: GLfloat -> Maybe GLuint -> IO()
cube w tex = do
  glBindTexture gl_TEXTURE_2D (fromJust tex)

  glBegin gl_QUADS -- start drawing a polygon (4 sided)
  glTexCoord2f   0    0
  glVertex3f   (-w) (-w)    w  -- bottom left of quad (Front)
  glTexCoord2f   1    0
  glVertex3f     w  (-w)    w  -- bottom right of quad (Front)
  glTexCoord2f   1    1 
  glVertex3f     w    w     w  -- top right of quad (Front)
  glTexCoord2f   0    1 
  glVertex3f   (-w)   w     w  -- top left of quad (Front)
  -- now the back
  glTexCoord2f   1    0 
  glVertex3f   (-w) (-w) (-w)  -- bottom right of quad (Back)
  glTexCoord2f   1    1 
  glVertex3f   (-w)   w  (-w)  -- top right of quad (Back)
  glTexCoord2f   0    1 
  glVertex3f     w    w  (-w)  -- top left of quad (Back)
  glTexCoord2f   0    0 
  glVertex3f     w  (-w) (-w)  -- bottom left of quad (Back)
  -- now the top
  glTexCoord2f   0    1
  glVertex3f   (-w)   w  (-w)  -- top left of quad (Top)
  glTexCoord2f   0    0  
  glVertex3f   (-w)   w    w   -- bottom left of quad (Top)
  glTexCoord2f   1    0  
  glVertex3f     w    w    w   -- bottom right of quad (Top)
  glTexCoord2f   1    1  
  glVertex3f     w    w  (-w)  -- top right of quad (Top)
  -- now the bottom
  glTexCoord2f   1    1  
  glVertex3f     w  (-w)   w   -- top right of quad (Bottom)
  glTexCoord2f   0    1  
  glVertex3f   (-w) (-w)   w   -- top left of quad (Bottom)
  glTexCoord2f   0    0 
  glVertex3f   (-w) (-w) (-w)  -- bottom left of quad (Bottom)
  glTexCoord2f   1    0  
  glVertex3f     w  (-w) (-w)  -- bottom right of quad (Bottom)
  -- now the right
  glTexCoord2f   1    0  
  glVertex3f     w  (-w) (-w)  -- bottom right of quad (Right)
  glTexCoord2f   1    1  
  glVertex3f     w    w  (-w)  -- top right of quad (Right)
  glTexCoord2f   0    1  
  glVertex3f     w    w    w   -- top left of quad (Right)
  glTexCoord2f   0    0  
  glVertex3f     w  (-w)   w   -- bottom left of quad (Right)
  -- now the left
  glTexCoord2f   0    0  
  glVertex3f   (-w) (-w) (-w)  -- bottom left of quad (Left)
  glTexCoord2f   1    0  
  glVertex3f   (-w)   w  (-w)  -- top left of quad (Left)
  glTexCoord2f   1    1  
  glVertex3f   (-w)   w    w   -- top right of quad (Left)
  glTexCoord2f   0    1 
  glVertex3f   (-w) (-w)   w   -- bottom right of quad (Left)
  
  glEnd
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
     --loadImage
     GLFW.defaultWindowHints
     -- get a 640 x 480 window
     -- initialize our window.
     Just win <- GLFW.createWindow 1280 720 "Lesson 1" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     -- register the function to do all our OpenGL drawing
     angle <- newIORef 0
     tex <- initGL win
     putStrLn $ show tex
     GLFW.setWindowRefreshCallback win (Just (drawScene tex angle))
     -- register the funciton called when our window is resized
     GLFW.setFramebufferSizeCallback win (Just resizeScene)
     -- register the function called when the keyboard is pressed.
     GLFW.setKeyCallback win (Just keyPressed)
     -- register window close handler
     GLFW.setWindowCloseCallback win (Just shutdown)
     -- start event processing engine
     forever $ do
       GLFW.pollEvents
       drawScene tex angle win
       GLFW.swapBuffers win

