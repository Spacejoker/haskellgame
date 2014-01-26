module GameTick where

--import Graphics.UI.GLUT
import Graphics.UI.GLUT (get, elapsedTime)
import Graphics.Rendering.OpenGL.Raw
import Control.Monad
import System.Random
import Data.IORef

import Cube
import Model
import StringUtil

makeLogics :: IORef GameState -> IO()
makeLogics gs = do

  gs' <- readIORef gs 
  let target = targetStr gs'
      common = commonPrefix (curStr gs') target
  nextTarget common target gs
  gs'' <- readIORef gs

  --update enemy positions:
  t <- get elapsedTime
  let dt = ((fromIntegral ((t0 gs') - t))::GLfloat)

  let enemies' = map (updateEnemies dt) (enemies gs'')

  writeIORef gs $! gs'' { commonStr = common, mode = nextMode gs'' t, t0 = t, enemies = enemies'}

updateEnemies :: GLfloat -> Enemy -> Enemy
updateEnemies dt e = e{enemyPos = (a, b)}
  where a = (fst $ enemyPos e) + (((speed e)*dt)::GLfloat)
        b = snd $ enemyPos e

nextMode :: GameState -> Int -> GameMode
nextMode gs t
  | heroHp gs <= 0 = GameOver
  | t0 gs == 0 = Title
  | otherwise = Play

nextTarget :: String -> String -> IORef GameState -> IO()
nextTarget cur target gs
  | (length cur) == (length target) = do
    word' <- getWord
    gs' <- readIORef gs
    let e' = (head $ enemies gs')
        pos = enemyPos e'
        pos' = (fst pos + 1, snd pos)

    writeIORef gs $! gs' { targetStr = word', curStr = "", score = (score gs') + 1, enemies = [e'{enemyPos = pos'}] }
  | otherwise = return ()

