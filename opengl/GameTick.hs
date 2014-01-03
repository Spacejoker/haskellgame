module GameTick where

import Graphics.UI.GLUT
import Control.Monad
import System.Random
import Data.IORef

import Points
import Cube
import Model
import StringUtil

attackEnemy :: GameState -> [Enemy]
attackEnemy gs = [Enemy (0,0) 10 10]
  -- | otherwise = []

checkWord :: IORef GameState -> String -> String -> String -> IO()
checkWord gs tStr cStr pStr
  | length cStr > length pStr = do
    word' <- getWord
    gs $~! \gs -> gs{targetStr = word', curStr = ""}
  | length tStr == length pStr = do
    word' <- getWord
    gs' <- get gs
    let enemies' = attackEnemy gs'
    gs $~! \gs -> gs{targetStr = word', curStr = "A", score = (score gs') + 1, enemies = enemies'}
  | otherwise = return ()

moveEnemy :: Int -> Enemy -> Enemy
moveEnemy dt e = e {enemyPos = enemyPos' }
  where enemyPos' = (fst p + 0.001*(fromIntegral dt), snd p)
        p = enemyPos e

enemiesAtPrincess :: [Enemy] -> Bool
enemiesAtPrincess [] = False
enemiesAtPrincess (x:xs) 
  | (fst $ enemyPos x) > 3 = True --1 + next
  | otherwise = next
    where next = enemiesAtPrincess xs


idle :: IORef GameState -> IdleCallback
idle gs = do
  gameLoop gs

gameLoop :: IORef GameState -> IdleCallback
gameLoop gs = do
  t <- get elapsedTime 
  gs' <- get gs
  let dt = t - (lastUpdate gs')
  let tStr = targetStr gs'
      cStr = reverse $ curStr gs'
      pStr = commonPrefix (targetStr gs') (reverse $ curStr gs')
  checkWord gs tStr cStr pStr

  --putStrLn $ show dt
  gs'' <- get gs
  rng <- getStdRandom (randomR (1,(6::Int)))
  let enemies' = map (moveEnemy dt) (enemies gs'')
  let gameOver' = enemiesAtPrincess (enemies gs'')
  --let enemies' = map (\(x, y) -> (x+0.01, y)) [(enemyPos $ head $ enemies gs')]
  -- set some values
  gs $~! \gs -> gs{lastUpdate = t, enemies = enemies', gameOver = (gameOver gs'') || gameOver'}
  postRedisplay Nothing
