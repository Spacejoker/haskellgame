module Draw where

import Model
import DrawWorldMap
import DrawFight

drawGamestate :: GameState -> IO ()
drawGamestate gs
  | gameMode gs == Model.Walking = drawWalkingMode gs
  | gameMode gs == Model.Fight = drawFight gs
  | gameMode gs == Model.AfterFight = return ()
