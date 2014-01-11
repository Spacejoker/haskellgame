module MenuRender where

import Graphics.Rendering.FTGL
import qualified Graphics.UI.GLFW as GLFW
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

displayTitle gs gx = do

  glClear $ fromIntegral  $  gl_COLOR_BUFFER_BIT
                         .|. gl_DEPTH_BUFFER_BIT
  setup3D

  glBindTexture gl_TEXTURE_2D (texCube gx)
 
  glRotatef 50 1 0 0
  glRotatef 25 0 2 0
  t <- get elapsedTime 
  let f = (\x -> (x, 2 + sin ((-x)+(fromIntegral t)/200.0)))
      fsin = map f [-5,-4..5]
  drawSinCubes fsin (Just $ texCube gx) (fromIntegral t)

  setup2D
  
  gs' <- readIORef gs 
  let mod = sin((fromIntegral t)/200)*0.3
      textColor = (0.2, 0.2, 0.3)
      chosenColor = (0.5+mod, 0.5+mod, 0.5+mod)
      xBase = 570
  renderTextMenu (font gx) 
               [("New Game", (xBase, 400)),( "Credits", (xBase + 12, 440)), ("  Quit", (xBase, 480))]
               textColor chosenColor 0 (menuChoice gs')
  glFlush

drawSinCubes :: [(GLfloat, GLfloat)] -> Maybe GLuint -> GLfloat -> IO()
drawSinCubes [] _ t = return ()
drawSinCubes ((a, b):xs) tex t = do
  glLoadIdentity  -- reset view
  glTranslatef a b (-6.0) 

  glRotatef (t*0.2) 1 0 0
  glRotatef (t*0.08) 0 2 0
  cube 0.2 tex
  drawSinCubes xs tex t

renderTextMenu :: Font -> [(String, (GLfloat, GLfloat))]-> (GLfloat, GLfloat, GLfloat) ->
                (GLfloat, GLfloat, GLfloat) -> Int -> Int -> IO()
renderTextMenu _ [] _ _ _ _ = return ()
renderTextMenu fnt ((s, (x, y)):xs)  unchosen chosen cur choice
  | cur == choice = do
    drawText fnt s (x, y) chosen
    renderTextMenu fnt xs unchosen chosen (cur+1) choice
  | otherwise = do
    drawText fnt s (x, y) unchosen
    renderTextMenu fnt xs unchosen chosen (cur+1) choice

-- old
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
  glColor3f 1 1 1 
  glTranslatef 0 0 (-5.0) 

  glLoadIdentity  -- reset view
  glColor3f 1 1 1 
  putStrLn "TOOO"
  putStrLn $ show $ font gx
  setFontFaceSize (font gx) 24 72
  renderFont (font gx) "Hello world!" All

  glFlush
