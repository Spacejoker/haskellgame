module Model where

data GameState = GameState {
  targetStr     :: String,
  nextTargetStr :: String,
  curStr        :: String,
  score        :: Int
}
