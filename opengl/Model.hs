module Model where

--import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw
--import Graphics.UI.GLUT (get, elapsedTime)
import Graphics.Rendering.FTGL
--import qualified Graphics.UI.GLFW as GLFW

data GameMode = Title | Play | GameOver | Credits
  deriving Show

modeFromInt 0 = Play
modeFromInt 1 = Credits
modeFromInt 2 = Title

data Enemy = Enemy {
  enemyPos      :: (GLfloat, GLfloat),
  enemyHp       :: Int,
  maxhp         :: Int,
  speed         :: GLfloat,
  spawnTime     :: Int
}

data GameState = GameState {
  targetStr     :: String,
  curStr        :: String,
  commonStr     :: String,
  score         :: Int,
  lastUpdate    :: Int,
  enemies       :: [Enemy],
  mode          :: GameMode,
  menuChoice    :: Int,
  t0            :: Int,
  heroHp        :: Int
}

  

data Graphics = Graphics {
  texCube       :: GLuint,
  font          :: Font
}

newGame :: Int -> GameState
newGame time = GameState "Link" "" "" 0 0 enemies Title 0 time 10
  where enemies = [newEnemy time]

newEnemy :: Int -> Enemy
newEnemy time = Enemy (0,0) 10 10 0.0003 time
