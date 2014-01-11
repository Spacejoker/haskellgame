module RenderUtil where

--import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw ( gluPerspective )
import Graphics.Rendering.FTGL

screenWidth :: GLdouble
screenWidth = 1280
screenHeight :: GLdouble
screenHeight = 720

drawText :: Font -> String -> (GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> IO()
drawText fnt s (x, y) (r, g, b) = do
  glColor3f r g b
  glRasterPos2f x y
  setFontFaceSize fnt 24 72
  renderFont fnt s All

setup3D :: IO()
setup3D = do
  glEnable gl_TEXTURE_2D
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45 (16.0/9.0) 0.1 100 
  glMatrixMode gl_MODELVIEW
  glEnable gl_DEPTH_TEST
  glLoadIdentity
  glColor3f 1 1 1

setup2D :: IO()
setup2D = do
  glMatrixMode gl_PROJECTION
  glDisable gl_TEXTURE_2D
  glLoadIdentity
  glOrtho 0.0 screenWidth screenHeight 0.0 (-1.0) 1.0
  glMatrixMode gl_MODELVIEW
  glDisable gl_DEPTH_TEST
  glLoadIdentity
  glColor3f 1 1 1

