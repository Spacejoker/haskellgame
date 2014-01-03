module Model where

data GameMode = Title | Play | GameOver | Credits
  deriving Show

data Enemy = Enemy {
  enemyPos :: (Float, Float),
  hp       :: Int,
  maxhp    :: Int
}

data GameState = GameState {
  targetStr     :: String,
  curStr        :: String,
  score         :: Int,
  lastUpdate    :: Int,
  enemies       :: [Enemy],
  mode          :: GameMode
}

newGame :: GameState
newGame = GameState "Link" "" 0 0 enemies Title
  where enemies = [Enemy (0,0) 10 10]
