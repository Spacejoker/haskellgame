module Event where

import Graphics.UI.SDL as SDL
import Model

getEvents :: IO Event -> [Event] -> IO [Event]
getEvents pEvent es = do
  e <- pEvent
  let hasEvent = e /= NoEvent
  if hasEvent
    then getEvents pEvent (e:es)
    else return (reverse es)

handleEvents :: [Event] -> GameState -> GameState
handleEvents [] gs = gs
handleEvents (x:xs) gs
  | gameMode gs == Model.Walking = handleEvents xs (handleWalkingEvent x gs)
  | gameMode gs == Model.Fight = handleEvents xs (handleFightEvent x gs)

handleFightEvent :: Event -> GameState -> GameState
handleFightEvent x gs =
  case x of
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs {gameActive = False}
    KeyDown (Keysym SDLK_a _ _ ) -> gs { gameMode = Model.AfterFight }
    _ -> gs

handleWalkingEvent :: Event -> GameState -> GameState
handleWalkingEvent x gs = 
  case x of
    KeyDown (Keysym SDLK_ESCAPE _ _) -> gs {gameActive = False}
    KeyDown (Keysym SDLK_RIGHT _ _) -> gs {player = player'}
      where player' = (player gs){ moveDirection = Model.Right, speed = Model.Slow }
    KeyDown (Keysym SDLK_LEFT _ _) -> gs {player = player'}
      where player' = (player gs){ moveDirection = Model.Left, speed = Model.Slow }
    KeyDown (Keysym SDLK_UP _ _) -> gs {player = player'}
      where player' = (player gs){ moveDirection = Model.Up, speed = Model.Slow }
    KeyDown (Keysym SDLK_DOWN _ _) -> gs {player = player'}
      where player' = (player gs){ moveDirection = Model.Down, speed = Model.Slow }
    _ -> gs
