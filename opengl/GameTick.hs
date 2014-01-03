module GameTick where

import Graphics.UI.GLUT
import Control.Monad
import System.Random
import Data.IORef

import Points
import Cube
import Model
import StringUtil

checkWord :: IORef GameState -> String -> String -> String -> IO()
checkWord gs tStr cStr pStr
  | length cStr > length pStr = do
    word' <- getWord
    gs $~! \gs -> gs{targetStr = word', curStr = ""}
  | length tStr == length pStr = do
    word' <- getWord
    gs' <- get gs
    gs $~! \gs -> gs{targetStr = word', curStr = "", score = (score gs') + 1}
  | otherwise = return ()

idle :: IORef GameState -> IdleCallback
idle gs = do

  t <- get elapsedTime 
  gs' <- get gs
  let dt = t - (lastUpdate gs')
  let tStr = targetStr gs'
      cStr = reverse $ curStr gs'
      pStr = commonPrefix (targetStr gs') (reverse $ curStr gs')
  checkWord gs tStr cStr pStr

  putStrLn $ show dt

  rng <- getStdRandom (randomR (1,(6::Int)))

  let enemies' = map (\(x, y) -> (x+0.01, y)) (enemies gs')
  -- set some values
  gs $~! \gs -> gs{lastUpdate = t, enemies = enemies'}
  postRedisplay Nothing
