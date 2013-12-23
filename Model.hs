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

data EnemyType = Rat | Slime

data Enemy = Enemy {
  enemyName :: String,
  enemyType :: EnemyType,
  maxhp :: Int,
  hp :: Int,
  maxmp :: Int,
  mp :: Int,
  level :: Int,
  curAnimation :: Animation
}

data Graphics = Graphics {
  tileSurface :: Surface,
  fightbg :: Surface,
  menumarker :: Surface,
  menubg :: Surface,
  enemyfire :: Surface,
  explosion :: Surface,
  walkDownSprite :: Surface,
  hitSprite :: Surface,
  ratSprite :: Surface
}

data Menu = Menu {
  name :: String,
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
  fnt :: Font,
  actions :: [String],
  enemies :: [Enemy]
}

data Animation = Animation {
  sheet :: Surface,
  width :: Int,
  height :: Int,
  frameCount :: Int,
  imageTime :: Int,
  lastSwitchTime :: Word32,
  currentImage :: Int,
  animPos :: Position,
  expire :: Maybe(Word32)
}

data Player = Player {
  moveDirection :: Direction,
  speed :: Speed,
  playerPos :: Position,
  animation :: Animation
}
