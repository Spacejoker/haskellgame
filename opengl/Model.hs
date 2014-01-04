module Model where

--import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw
--import qualified Graphics.UI.GLFW as GLFW

data GameMode = Title | Play | GameOver | Credits
  deriving Show

data Enemy = Enemy {
  enemyPos :: (Float, Float),
  hp       :: Int,
  maxhp    :: Int,
  speed    :: Float
}

data GameState = GameState {
  targetStr     :: String,
  curStr        :: String,
  score         :: Int,
  lastUpdate    :: Int,
  enemies       :: [Enemy],
  mode          :: GameMode
}

data Graphics = Graphics {
  texCube       :: GLuint
}

newGame :: GameState
newGame = GameState "Link" "" 0 0 enemies Title
  where enemies = [newEnemy]

newEnemy :: Enemy
newEnemy = Enemy (-10,0) 10 10 1.0
