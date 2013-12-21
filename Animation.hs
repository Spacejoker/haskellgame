module Animation where

import Graphics.UI.SDL as SDL

import Data.Word

import Model

drawGamestate :: GameState -> IO ()
drawGamestate gs = do
  drawAnimations $ ((animation (player gs)) : animations gs)

drawAnimations :: [Animation] -> IO ()
drawAnimations x = do
  s <- getVideoSurface
  blitAnimations x s
  SDL.flip s

blitAnimations :: [Animation] -> Surface -> IO()
blitAnimations [] _ = do 
  return ()
blitAnimations (x:xs) s = do
  let startx = (width x) * (currentImage x)
  let xpos = xVal $ animPos x
  let ypos = yVal $ animPos x
  blitSurface (sheet x) (Just (Rect startx 0 ((width x)) 33)) s (Just (Rect (floor xpos) (floor ypos) 1000 1000))
  blitAnimations xs s

nextFrame :: Animation -> Word32 -> Animation
nextFrame x t = x {lastSwitchTime = (fromIntegral t), currentImage = ((currentImage x)+1) `mod` frameCount x}

updateAnimations :: [Animation] -> Word32 -> [Animation]
updateAnimations [] _ = []
updateAnimations (x:xs) t
  | t - (lastSwitchTime x) > (fromIntegral $ imageTime x)  = ( nextFrame x t : updateAnimations xs t)
  | otherwise = (x : updateAnimations xs t)

