module MenuRender where

--ssimport Graphics.Rendering.OpenGL.GL as GL --(textureBinding, ($=), Texture2D)
import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Data.Bits ( (.|.) )
import System.Exit ( exitWith, ExitCode(..) )
import Control.Monad ( forever )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Foreign ( withForeignPtr, plusPtr, peek, alloca )
import qualified Data.ByteString.Internal as BSI
import Control.Monad
import System.Random
import Data.IORef

import Cube
import Model
import StringUtil
import GameTick
import RenderUtil

renderMenu :: IORef GameState -> Graphics -> IO()
renderMenu iogs gx = do
  putStrLn "Render"
  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  glTranslatef 0 0 (-6.0) 

  glRotatef 50 1 0 0
  glRotatef 25 0 2 0

  cube 1 (Just $ texCube gx)
  --writeIORef angle  $! angle_ + 0.1
  --angle $~! \angle -> (angle + 0.1)

  glFlush

