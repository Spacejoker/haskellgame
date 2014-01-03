module Model where

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
  gameOver      :: Bool
}
