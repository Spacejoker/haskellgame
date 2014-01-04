module Cube where

import qualified Graphics.UI.GLFW as GLFW
-- everything from here starts with gl or GL
import Graphics.Rendering.OpenGL.Raw
import Data.Maybe

cube :: GLfloat -> Maybe GLuint -> IO()
cube w tex = do
  --glBindTexture gl_TEXTURE_2D (fromJust tex)

  glBegin gl_QUADS -- start drawing a polygon (4 sided)
  glColor3f 1 1 0
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
