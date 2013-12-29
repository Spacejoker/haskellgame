module DrawFight where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Data.Word
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map

import Model
import Animation
import DrawMenu
 
showHp :: [Enemy] -> Surface -> Font -> IO (Bool)
showHp [] _ _ = return (True)
showHp (x:xs) s fnt = do
  let str = "HP"
  title <- renderTextSolid fnt str (Color 20 0 0)
  let pos = animPos $ curAnimation x
  putStrLn $ show $ xVal pos
  blitSurface title Nothing s (Just (Rect (floor $ xVal pos) (floor $ (yVal pos - 20)) 200 200))
  
 
drawFight :: GameState -> IO ()
drawFight gs = do
  s <- getVideoSurface
  blitSurface (fightbg $ gx gs) Nothing s Nothing

  --blitSurface (enemyfire $ gx gs) Nothing s (Just (Rect 200 100 0 0))
  
  showHp (enemies gs) s (fnt gs)

  blitAnimations (map curAnimation (enemies gs)) s (Position 0 0)

  blitAnimations (animations gs) s (Position 0 0)
  drawMenu gs s
  SDL.flip s
