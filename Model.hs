module Model where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Data.Word
import Data.Tiled

import System.Random

data Direction = None | Up | Down | Left | Right
  deriving Eq
data Speed = Stop | Slow | Fast
  deriving Eq
data Position = Position {
  xVal :: Float,
  yVal :: Float
}

data Mode = Walking | Fight | AfterFight
  deriving (Eq, Show)

data Graphics = Graphics {
  tileSurface :: Surface,
  fightbg :: Surface,
  menumarker :: Surface,
  menubg :: Surface,
  enemyfire :: Surface
}

data Menu = Menu {
  choice :: Int,
  labels :: [String],
  menuPos :: Position
}

data GameState = GameState{
  gameActive :: Bool,
  animations :: [Animation],
  time :: Word32,
  player :: Player,
  currentMap :: TiledMap,
  cameraPos :: Position,
  gameMode :: Mode,
  rng :: StdGen,
  nextFight :: Int,
  gx :: Graphics,
  menu :: Menu,
  fnt :: Font
}

data Animation = Animation {
  sheet :: Surface,
  width :: Int,
  frameCount :: Int,
  imageTime :: Int,
  lastSwitchTime :: Word32,
  currentImage :: Int,
  animPos :: Position
}

data Player = Player {
  moveDirection :: Direction,
  speed :: Speed,
  playerPos :: Position,
  animation :: Animation
}
