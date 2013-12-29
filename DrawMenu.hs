module DrawMenu where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Data.Word
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map

import Model
import Animation

drawLabels :: [String] -> Position -> Surface -> Font -> IO()
drawLabels [] _ _ _ = return ()
drawLabels (x:xs) pos s fnt = do
  title <- renderTextSolid fnt x (Color 20 0 0)
  blitSurface title Nothing s (Just (Rect (floor $ xVal pos) (floor $ yVal pos) 200 200))
  drawLabels xs (Position ((xVal pos)) ((yVal pos) + 30)) s fnt

drawMenu :: GameState -> Surface -> IO ()
drawMenu gs s = do
  let menu' = menu gs
  let pos' = menuPos menu'
  let gx' = gx gs
  blitSurface (menubg gx') Nothing s (Just (Rect (floor $ xVal pos') (floor $ yVal pos') 20 20 ))
  blitSurface (menumarker gx') Nothing s (Just (Rect (floor $ xVal pos') ((floor $ yVal pos') + 10 +(choice menu') *30) 20 20 ))
  drawLabels (labels menu') (Position (((xVal pos') +10.0)) (((yVal pos') + 10.0))) s (fnt gs)
  
  return ()
