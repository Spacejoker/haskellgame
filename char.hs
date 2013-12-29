data Class = Fighter | Rogue | Mage
data Element = Physical | Fire | Ice | Earth | Water
  deriving Eq

data Res = Res {
  fireRes :: Int,
  coldRes :: Int,
  earthRes :: Int,
  waterRes :: Int
}
  deriving Show

data Damage = Damage {
  amount :: Int,
  element :: Element
}

data Stat = Stat {
  level :: Int,
  maxhp :: Int,
  hp :: Int,
  maxmp :: Int,
  mp :: Int,
  res :: Res
}
  deriving Show

makeStat :: Int -> Class -> Stat
makeStat level Rogue = genStat level 1.5 2.8 0.6 0.2

genStat :: Int -> Float -> Float -> Float -> Float -> Stat
genStat level str dex vit ene = Stat level maxhp maxhp maxmp maxmp res
  where maxhp = ((floor (vit*10))*level)
        maxmp = ((floor (ene*10))*level)
	res = Res 0 0 0 0

applyResistance :: Damage -> Res -> Int
applyResistance dmg res = 0

main = do
  putStrLn $ show $ makeStat 10 Rogue

