module Logics where

import Graphics.UI.SDL as SDL

import Data.Word
import Data.Maybe
import Data.Tiled

import Model
import Animation
import LogicWalking

import System.Random

performFightActions :: GameState -> [String] -> Word32 -> IO(GameState)
performFightActions gs [] _ = return (gs)
performFightActions gs ("Attack":xs) t = do
  putStrLn "Attacking"
  let hitAnim = Animation (hitSprite $ gx gs) 70 30 3 100 t 0 (Position 0 0) (Just (t + 300))
  return (gs {animations = (hitAnim : (animations gs))})

updateGamestate :: Model.Mode -> GameState -> Word32 -> Word32 -> IO(GameState)
updateGamestate Model.Walking gs t dt = do
  let animations' = updateAnimations ((map curAnimation (enemies gs)) ++ (animations gs)) t
  let player' = updatePlayer (player gs) t dt
  let cameraPos' = updateCamera (cameraPos gs) (playerPos $ player gs) (fromIntegral $ mapWidth $ currentMap gs) (fromIntegral $ mapHeight $ currentMap gs)
  let gs' = checkForFight gs t
  return (gs' { animations = animations', player = player', cameraPos = cameraPos'})

updateGamestate Model.AfterFight gs t dt = do
  let gs' = (setUpNextFight gs ( fromIntegral (t+1000) )) { gameMode = Model.Walking, actions = [], enemies = [] }
  return (gs')

updateGamestate Model.Fight gs t dt = do
  gs' <- performFightActions gs (actions gs) t
  let animations' = updateAnimations (animations gs') t
  return (gs' {actions = [], animations = filter (\x -> fromJust ( expire x ) > t) (animations')})

activateMenuOption :: GameState -> String -> Int -> GameState
activateMenuOption gs "Fight" 0 = gs { actions = actions'}
  where actions' = ("Attack" : actions gs)
activateMenuOption gs "Fight" 1 = gs {gameMode = Model.AfterFight }
