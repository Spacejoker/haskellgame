module Model where

--import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.FTGL
--import qualified Graphics.UI.GLFW as GLFW

data GameMode = Title | Play | GameOver | Credits
  deriving Show

modeFromInt 0 = Play
modeFromInt 1 = Credits
modeFromInt 2 = Title

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
  mode          :: GameMode,
  menuChoice    :: Int
}

data Graphics = Graphics {
  texCube       :: GLuint,
  font          :: Font
}

newGame :: GameState
newGame = GameState "Link" "" 0 0 enemies Title 0
  where enemies = [newEnemy]

newEnemy :: Enemy
newEnemy = Enemy (-10,0) 10 10 1.0
