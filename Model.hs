module Model where

import Graphics.UI.SDL as SDL

import Data.Word
import Data.Tiled

data Direction = None | Up | Down | Left | Right
  deriving Eq
data Speed = Stop | Slow | Fast
  deriving Eq
data Position = Position {
  xVal :: Float,
  yVal :: Float
}

data Mode = Walking | Fight
  deriving Eq

data GameState = GameState{
  gameActive :: Bool,
  animations :: [Animation],
  time :: Word32,
  player :: Player,
  currentMap :: TiledMap,
  tileSurface :: Surface,
  cameraPos :: Position,
  gameMode :: Mode
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
