module Draw where

import Model
import DrawWorldMap
import DrawFight
import DrawMenu

drawGamestate :: GameState -> Mode -> IO ()
drawGamestate gs Model.Walking = do
  drawWalkingMode gs
  drawMenu gs (hasMenu gs)

drawGamestate gs Model.AfterFight = do
  return ()

drawGamestate gs Model.Fight = do
  drawFight gs
  drawMenu gs (hasMenu gs)
