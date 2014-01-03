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

color3 :: Double -> Double -> Double -> Color3 Double
color3 = Color3

vert2D :: (Double, Double) -> Vertex3 Double
vert2D (x,y) = Vertex3 x y 0
