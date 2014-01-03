module RenderUtil where

import Graphics.UI.GLUT

drawString :: String -> Vector3 GLfloat-> Color3 GLfloat -> IO()
drawString s pos color' = do
  let sc = (0.01::GLfloat)
  color color'
  loadIdentity
  scale sc sc sc
  translate $ pos
  renderString MonoRoman $ s
