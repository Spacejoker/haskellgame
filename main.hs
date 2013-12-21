import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import System.Random

data GameState = GameState{
  gameActive :: Bool
}

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "RPG" "RPG" 

  enableKeyRepeat 500 30

  fnt <- openFont "font.ttf" 30
  sheet <- SDLi.load "sheet.png"
 
  gameLoop sheet (GameState True)

gameLoop :: Surface -> GameState-> IO ()
gameLoop image gs = do

  events <- getEvents pollEvent []
  let gs' = handleEvents events gs

  s <- getVideoSurface

  blitSurface image Nothing s (Just (Rect 245 370 350 460))
  SDL.flip s

  if gameActive gs'
    then gameLoop image gs'
    else return ()

getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

handleEvents :: [Event] -> GameState -> GameState
handleEvents [] gs = gs
handleEvents (x:xs) gs = handleEvents xs (handleEvent x gs)

handleEvent :: Event -> GameState -> GameState
handleEvent x gs = 
  case x of
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs {gameActive = False}
    _ -> gs
