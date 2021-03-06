module StringUtil where

import System.Random

commonPrefix :: String -> String -> String
commonPrefix [] _ = ""
commonPrefix _ [] = ""
commonPrefix (x:xs) (y:ys)
  | x == y = (x:commonPrefix xs ys)
  | otherwise =  ""

turnLength :: Int
turnLength = 5

getWord :: IO(String)
getWord = do
  rng <- getStdRandom (randomR (0,((length allWords)-1::Int)))
  let w = allWords !! rng
  return (w)

ff7Words :: [String]
ff7Words = [
            "No. After all, I did work for Shinra, y'know."]

allWords :: [String]
allWords = ["Heart container",
            "Heart",
            "Fairy",
            "Clock",
            "Rupy",
            "Sword",
            "White sword",
            "Magical sword",
            "Magical shield",
            "Boomerang",
            "Magical boomerang",
            "Bomb",
            "Bow",
            "Arrow",
            "Silver arrow",
            "Blue candle",
            "Red candle",
            "Blue ring",
            "Red ring",
            "Power bracelet",
            "Recorder",
            "Raft",
            "Stepladder",
            "Magical rod",
            "Book of magic",
            "Key",
            "Magical key",
            "Map",
            "Compass",
            "Triforce",
            "Life potion",
            "Letter",
            "Food"]
