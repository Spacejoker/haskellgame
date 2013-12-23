module Animation where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Data.Word
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map

import Model



blitAnimations :: [Animation] -> Surface -> Position -> IO()
blitAnimations [] _ _ =  return ()
blitAnimations (x:xs) s camera = do
  let startx = (width x) * (currentImage x)
  let xpos = xVal $ animPos x
  let ypos = yVal $ animPos x
  blitSurface (sheet x) (Just (Rect startx 0 ((width x)) 33)) s (Just (Rect (floor ( xpos - (xVal camera))) (floor (ypos - (yVal camera))) 1000 1000))
  blitAnimations xs s camera

nextFrame :: Animation -> Word32 -> Animation
nextFrame x t = x {lastSwitchTime = (fromIntegral t), currentImage = ((currentImage x)+1) `mod` frameCount x}

updateAnimations :: [Animation] -> Word32 -> [Animation]
updateAnimations [] _ = []
updateAnimations (x:xs) t
  | t - (lastSwitchTime x) > (fromIntegral $ imageTime x)  = ( nextFrame x t : updateAnimations xs t)
  | otherwise = (x : updateAnimations xs t)

