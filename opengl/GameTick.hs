module GameTick where

import Graphics.UI.GLUT
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
  t <- get elapsedTime

  writeIORef gs $! gs'' { commonStr = common, mode = nextMode gs'' t}

nextMode :: GameState -> Int -> GameMode
nextMode gs t
  | (t0 gs) + turnLength*1000 - t <= 0 = GameOver
  | t0 gs == 0 = Title
  | otherwise = Play

nextTarget :: String -> String -> IORef GameState -> IO()
nextTarget cur target gs
  | (length cur) == (length target) = do
    word' <- getWord
    gs' <- readIORef gs
    writeIORef gs $! gs' { targetStr = word', curStr = "", score = (score gs') + 1 }
  | otherwise = return ()

idle :: IORef GameState -> IdleCallback
idle gs = do
  gs' <- get gs
  delegateLoop (mode gs') gs

attackEnemy :: GameState -> [Enemy]
attackEnemy gs = [newEnemy]

checkWord :: IORef GameState -> String -> String -> String -> IO()
checkWord gs tStr cStr pStr
  | length cStr > length pStr = do
    word' <- getWord
    gs $~! \gs -> gs{targetStr = word', curStr = ""}
  | length tStr == length pStr = do
    word' <- getWord
    gs' <- get gs
    let enemies' = attackEnemy gs'
    gs $~! \gs -> gs{targetStr = word', curStr = "", score = (score gs') + 1, enemies = enemies'}
  | otherwise = return ()

moveEnemy :: Int -> Enemy -> Enemy
moveEnemy dt e = e {enemyPos = enemyPos' }
  where enemyPos' = (fst p + 0.001*(fromIntegral dt)*(speed e), snd p)
        p = enemyPos e

enemiesAtPrincess :: [Enemy] -> GameMode
enemiesAtPrincess [] = Play
enemiesAtPrincess (x:xs) 
  | (fst $ enemyPos x) > 3 = GameOver --1 + next
  | otherwise = next
    where next = enemiesAtPrincess xs

delegateLoop Play gs = gameLoop gs
delegateLoop _ _ = do
  postRedisplay Nothing

deltaTime :: Int -> Int -> Int
deltaTime _ 0 = 0
deltaTime t t0 = t - t0

gameLoop :: IORef GameState -> IdleCallback
gameLoop gs = do
  t <- get elapsedTime 
  gs' <- get gs
  let dt = deltaTime t (lastUpdate gs')
  let tStr = targetStr gs'
      cStr = reverse $ curStr gs'
      pStr = commonPrefix (targetStr gs') (reverse $ curStr gs')

  checkWord gs tStr cStr pStr

  gs'' <- get gs
  rng <- getStdRandom (randomR (1,(6::Int)))
  let enemies' = map (moveEnemy dt) (enemies gs'')
  let mode' = enemiesAtPrincess (enemies gs'')

  gs $~! \gs -> gs{lastUpdate = t, enemies = enemies', mode = mode'}
  postRedisplay Nothing
