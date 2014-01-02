module Points where

import Graphics.UI.GLUT

--points' :: [(GLfloat,GLfloat,GLfloat)]
--points' = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

points :: Int -> [(GLfloat, GLfloat, GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2 * pi * k/n'), 0) | k <- [0..(n'-1)]]
  where n' = fromIntegral n
--points n = [ ( k/3.0, k/3.0, 0) | k <- [0..(n'-1)]]
  --where n' = fromIntegral n
