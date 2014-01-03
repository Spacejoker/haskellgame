module StringUtil where

commonPrefix :: String -> String -> String
commonPrefix [] _ = ""
commonPrefix _ [] = ""
commonPrefix (x:xs) (y:ys)
  | x == y = (x:commonPrefix xs ys)
  | otherwise =  ""
