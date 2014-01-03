module Model where

data GameState = GameState {
  targetStr     :: String,
  nextTargetStr :: String,
  curStr        :: String,
  score         :: Int,
  lastUpdate    :: Int,
  enemies       :: [(Float, Float)]
}
