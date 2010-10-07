module GOConst where

import Util (infinite)

powerUpLimits :: [Int]
powerUpLimits = [5,1,1,1,4,1]

-- Cuteness to add later
-- konamiCommand = [upButton,upButton,downButton,downButton,leftButton,rightButton,leftButton,rightButton,missileButton,shotButton]

gaugeOfMissile,gaugeOfDouble,gaugeOfLaser,gaugeOfShield :: Int
gaugeOfMissile = 1
gaugeOfDouble  = 2
gaugeOfLaser   = 3
gaugeOfShield  = 5

stageClearTime :: Int
stageClearTime = 7800

-- these lists are game rank modifiers.
bacterianShotSpeedList, duckerShotWay, jumperShotFactor, grashiaShotSpeedFactor :: [Double]
bacterianShotSpeedList = [8,4,6,8] ++ cycle [12,8]
duckerShotCount = [2,1,1,3] ++ repeat 2
duckerShotWay = [1,1,2,1] ++ cycle [2,2]
jumperShotFactor = [0.5,0.5,0.5,0.5] ++ cycle [0.8,0.5]
grashiaShotSpeedFactor = [1,1,1,1] ++ cycle [1,0.6]

flyerHitBack, particleHitBack, powerUpCapsuleHitBack, scrambleHatchHitBack, treasure, turnGearHitBack :: [Bool]
flyerHitBack = [False,False,False] ++ repeat True
particleHitBack = True:repeat False
powerUpCapsuleHitBack = [False,False,False,False] ++ cycle [False,True]
scrambleHatchHitBack = [False,False,False,False] ++ cycle [False,True]
treasure = [False,False,False,False] ++ cycle [False,True]
turnGearHitBack = [False,False,False] ++ repeat True

duckerShotCount, flyerShotInterval, grashiaShotHalt, grashiaShotInterval, inceptorShotInterval, jumperShotWay, landRollShotInterval, scrambleHatchLaunchLimitAge :: [Int]
flyerShotInterval = [30,infinite,30,15] ++ cycle [15,15]
grashiaShotHalt = [50,100,50,50] ++ cycle [0,0]
grashiaShotInterval = [30,60,30,30] ++ cycle [15,5]
inceptorShotInterval = [45,infinite,60,45] ++ cycle [45,45]
jumperShotWay = [16,4,8,16] ++ cycle [24,32]
landRollShotInterval = [60,120,60,60] ++ cycle [30,60]
scrambleHatchLaunchLimitAge = [400,200,400,400] ++ cycle [600,400]

shotSpeed,laserSpeed,laserBreadth :: Double
shotSpeed = 25
laserSpeed = 60
laserBreadth = 20

vicViperSize :: Double
vicViperSize = 6

shieldPlacementMargin,shieldHitMargin :: Double
shieldPlacementMargin = 5
shieldHitMargin = 10

shieldMaxHp, hatchHP :: Int
shieldMaxHp = 16
hatchHP = 15

diamondBombSize,smallBacterianSize,hatchHeight :: Double
diamondBombSize = 6
hatchHeight = 35
smallBacterianSize = 16
