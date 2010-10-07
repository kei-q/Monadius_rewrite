module GameObject where

import Data.Complex
import Data.Array

import GLWrapper (Color3)
import Util
import GOConst

newtype Monadius = Monadius (GameVariables,[GameObject])

getVariables :: Monadius -> GameVariables
getVariables (Monadius (vs,_))=vs


data GameVariables = GameVariables {
  totalScore :: Int,hiScore :: Int ,flagGameover :: Bool,
  nextTag :: Int, gameClock :: Int,baseGameLevel :: Int,playTitle :: Maybe String
  } deriving Eq


data GameObject = -- objects that are actually rendered and moved.
  VicViper{ -- player's fighter.
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,hp :: Int,
    trail :: [Complex Double],
    speed :: Double,
    powerUpPointer :: Int,
    powerUpLevels :: Array Int Int,
    reloadTime :: Int,weaponEnergy :: Int,
    ageAfterDeath :: Int
    } |
  Option{ -- trailing support device.
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,
    optionTag :: Int,
    reloadTime :: Int,weaponEnergy :: Int} |
  StandardMissile{
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,hp :: Int,mode :: Int,
    velocity :: Complex Double,parentTag :: Int,probe :: GameObject } | -- missile that fly along the terrain
  Probe{ -- this lets missile to fly along the terrain
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,hp :: Int
  } |
  StandardRailgun{
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,hitDispLand :: Shape,hp :: Int,
    velocity :: Complex Double,parentTag :: Int } | -- normal & double shot
  StandardLaser{
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,hitDispLand :: Shape,hp :: Int,
    velocity :: Complex Double,parentTag :: Int,age :: Int } | -- long blue straight laser
  Shield{
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,hitDispLand :: Shape,hp :: Int,
    settled :: Bool,size :: Double,placement :: Complex Double,
    angle :: Double,omega :: Double} |  -- solid state of Reek power that protects enemy atacks
  PowerUpCapsule{
    tag :: Maybe Int,position :: Complex Double,hitDisp :: Shape,hp :: Int,age :: Int} |
  PowerUpGauge{
    tag :: Maybe Int,position :: Complex Double} |

  DiamondBomb{
    tag :: Maybe Int,position :: Complex Double,velocity :: Complex Double,
    hitDisp :: Shape,hp :: Int,age :: Int} |  -- Bacterian's most popular warhead
  TurnGear{
    tag :: Maybe Int,position :: Complex Double,velocity :: Complex Double,
    hitDisp :: Shape,hp :: Int,age :: Int,mode :: Int,
    managerTag :: Int} |  -- one of small Bacterian lifeforms, often seen in a squad.
  SquadManager{
    tag :: Maybe Int,position :: Complex Double,interval :: Int,age :: Int,
    bonusScore :: Int,currentScore :: Int,
    members :: [GameObject],items :: [GameObject]
  } |
  -- 1. generates objects contained in <members> with <interval>, one at each time.
  -- 2. sticks to one of the still-alive troop members.
  -- 3. counts up <currentScore> every time when one of the squad members are destroyed by lack of hp.
  -- 4. doesn't count up <currentScore> if a squad member are destroyed by scrolling out.
  -- 5. dies when all squad members were destroyed. at this time,
  --        releases <items> if <currentScore> >= <bonusScore>, or
  --        doesn't ,if not.
  Jumper{
    tag :: Maybe Int,position :: Complex Double,velocity :: Complex Double,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,gravity :: Complex Double,
    touchedLand :: Bool,jumpCounter :: Int
  } | -- dangerous multi way mine dispenser.

  Grashia{
    tag :: Maybe Int,position :: Complex Double,velocity :: Complex Double,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,gravity :: Complex Double,
    gunVector :: Complex Double,mode :: Int
  } | -- fixed antiaircraft cannon.

  Ducker{
    tag :: Maybe Int,position :: Complex Double,velocity :: Complex Double,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,gVelocity :: Complex Double,
    charge :: Int,vgun :: Complex Double,touchedLand :: Bool
  } | -- 2-feet mobile land to air attack device.

  Flyer{
    tag :: Maybe Int,position :: Complex Double,velocity :: Complex Double,
    hitDisp :: Shape,hp :: Int,age :: Int,hasItem :: Bool,mode :: Int
  } | -- Baterian's standard interceptor.

  ScrambleHatch{
    tag :: Maybe Int,position :: Complex Double,gateAngle :: Double,gravity :: Complex Double,
    hitDisp :: Shape,hp :: Int,age :: Int,launchProgram :: [[GameObject]]
  } | -- Where Baterian larvae spend last process of maturation.

  LandScapeBlock{
    tag :: Maybe Int, position :: Complex Double,hitDisp :: Shape,velocity :: Complex Double
  } | -- landscape that just look like, and hit like its hitDisp.

  Particle{
    tag :: Maybe Int, position :: Complex Double, velocity :: Complex Double,
    size :: Double,particleColor :: Color3 Double,age :: Int,decayTime :: Double,expireAge :: Int
  } | -- multi purpose particles that vanishes after expireAge.

  Star{
    tag :: Maybe Int, position :: Complex Double,particleColor :: Color3 Double
  } | -- background decoration
  SabbathicAgent{
    tag :: Maybe Int, fever :: Int
  } | -- generates many flyers for additional fun if there are none of them
  DebugMessage {tag :: Maybe Int,debugMessage :: String} |
  ScoreFragment{tag :: Maybe Int,score :: Int}


freshDiamondBomb, freshFlyer, freshInterceptor, freshOption, freshPowerUpCapsule, freshPowerUpGauge, freshShield, freshStalk, freshStandardLaser, freshStandardMissile, freshStandardRailgun, freshTurnGear, freshTurnGearSquad, freshVicViper :: GameObject
freshDiamondBomb = DiamondBomb{tag=Nothing,position=0:+0,velocity=0:+0,hp=1,hitDisp=Circular (0:+0) diamondBombSize,age=0}
freshFlyer = Flyer{tag=Nothing,position=0:+0,velocity=(-3):+0,hitDisp=Circular 0 smallBacterianSize,hp=1,age=0,hasItem=False,mode=0}
freshInterceptor = freshFlyer{mode=1,velocity = 0:+0}
freshOption = Option{tag = Nothing, position=0:+0, hitDisp = Circular (0:+0) 0,optionTag = 0,reloadTime=0,weaponEnergy=100}
freshPowerUpCapsule = PowerUpCapsule{tag = Nothing, hitDisp = Circular (0:+0) 30,position = 0:+0,hp=1,age=0}
freshPowerUpGauge = PowerUpGauge{tag=Nothing, position = (-300):+(-240)}
freshShield = Shield{tag=Nothing,position=380:+0,hitDisp=Circular (0:+0) 0,hitDispLand=Circular (0:+0) 0,hp=shieldMaxHp,settled=False,size=0,placement=0:+0,angle=0,omega=0}
freshStalk = freshFlyer{mode=10,velocity = (-2):+0}
freshStandardLaser = StandardLaser{tag=Nothing,position=0:+0,hitDisp=Rectangular (laserSpeed/(-2):+(-laserBreadth)) (laserSpeed/2:+laserBreadth),hitDispLand = Rectangular (laserSpeed/(-2):+(-vicViperSize)) (laserSpeed/2:+vicViperSize),velocity=laserSpeed:+0,hp=1,parentTag=0,age=0}
freshStandardMissile = StandardMissile{tag=Nothing,position=0:+0,hitDisp=Circular 0 7,velocity=0:+0,hp=1,parentTag=0,probe=Probe{tag=Nothing,position=0:+0,hitDisp=Circular (0:+(-5)) 12,hp=1},mode=0}
freshStandardRailgun = StandardRailgun{tag=Nothing,position=0:+0,hitDisp=Circular 0 12,hitDispLand = Circular (0:+0) vicViperSize,velocity=shotSpeed:+0,hp=1,parentTag=0}
freshTurnGear = TurnGear{tag=Nothing,position=0:+0,velocity=0:+0,hp=1,hitDisp=Circular (0:+0) smallBacterianSize,age=0,managerTag=0,mode=0}
freshTurnGearSquad = SquadManager{tag=Nothing,position=0:+0, interval=10, age=0,
  bonusScore=squadSize, currentScore=0, members = replicate squadSize freshTurnGear,items=[freshPowerUpCapsule]} where
  squadSize=6
freshVicViper = VicViper{tag = Nothing, position = 0:+0, hitDisp = Circular (0:+0) vicViperSize,hp=1, trail = repeat $ 0:+0,
      speed = 1,powerUpPointer=(-1),powerUpLevels=array (0,5) [(x,0)|x<-[0..5]],reloadTime=0,weaponEnergy=100,
      ageAfterDeath = 0}

freshDucker :: Double -> GameObject
freshDucker vg = Ducker{tag = Nothing, position = 0:+0, velocity= 0:+0, hitDisp = Circular (0:+0) smallBacterianSize, hp = 1,
  age = 0, hasItem = False, gVelocity = 0:+(8*vg),charge = 0, vgun = 0:+0,touchedLand=False}
freshScrambleHatch :: Double -> GameObject
freshScrambleHatch sign = ScrambleHatch{tag=Nothing,position=0:+0,hitDisp=regulate $ Rectangular ((-45):+0) (45:+(hatchHeight*(-sign))),gravity=(0:+sign),hp=hatchHP,age=0,
  launchProgram = cycle $ replicate 40 [] ++ (concat.replicate 6) ([[freshInterceptor{velocity = 0:+(-6)*sign}]]++replicate 9 []),gateAngle=0
  }

freshVolcano :: Double -> GameObject
freshVolcano grvty = LandScapeBlock{tag=Nothing,position=0:+0,velocity=0:+0,hitDisp=
  Shapes $ map (regulate.(\i -> Rectangular ((120 - 33*i + 2*i*i):+ sign*30*i) ((33*i - 2*i*i - 120) :+ sign*30*(i+1)) ) ) [0..4]}
  where sign = (-grvty)

freshTable :: Double -> GameObject
freshTable grvty =  LandScapeBlock{tag=Nothing,position=0:+0,velocity=0:+0,hitDisp= Shapes $ map (regulate.(\i -> Rectangular ((-2**(i+3)+shiftSinePi i):+sign*30*i) ((2**(i+3)+shiftSinePi i) :+sign*30*(i+1)))) [0..4]
} where
    sign = (- grvty)

    shiftSinePi :: (Floating a) => a -> a
    shiftSinePi a = 5 * sin (a*0.5*pi)

freshGrashia, freshJumper, freshLandRoll :: Double -> GameObject
freshGrashia sign= Grashia{tag=Nothing,position=0:+0,velocity=0:+0,
  hitDisp=Circular 0 smallBacterianSize,hp=1,age=0,hasItem=False,gravity=(0:+sign),gunVector=0:+0,mode=0}
freshJumper sign=Jumper{tag=Nothing,position=0:+0,velocity=0:+0,
                    hitDisp=Circular 0 smallBacterianSize,hp=1,age=0,hasItem=False,gravity=(0:+0.36*sign),touchedLand=False,jumpCounter=0}
freshLandRoll sign = (freshGrashia sign){mode=1}

freshLandScapeGround, freshSabbathicAgent :: GameObject
freshLandScapeGround = LandScapeBlock{tag=Nothing,position=0:+0,velocity=0:+0,hitDisp=Rectangular ((-158):+(-20)) (158:+20)}
freshSabbathicAgent = SabbathicAgent{tag=Nothing,fever=1}

freshScore :: Int -> GameObject
freshScore point = ScoreFragment{tag=Nothing,score = point}

