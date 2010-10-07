module Monadius (
                 Monadius(..) ,initialMonadius,getVariables,
                 GameVariables(..),
                 shotButton,missileButton,powerUpButton,upButton,downButton,leftButton,rightButton,selfDestructButton
                ) where

import Data.Array ((!), Array(), array)
import Data.Complex -- ((:+))
import Data.List (intersect, find)
import Data.Maybe (fromJust, isJust, isNothing)
import Prelude hiding (catch)

import Game
import Util

import GLWrapper

import GameObject
import GOConst
import GameObjectRender
import Stage

instance Game Monadius where
  update = updateMonadius
  render = renderMonadius
  isGameover = isMonadiusOver


data HitClass = BacterianShot |
                BacterianBody |
                LaserAbsorber |
                MetalionShot |
                MetalionBody |
                ItemReceiver |
                PowerUp |
                LandScape
                deriving(Eq)

data WeaponType = NormalShot | Missile | DoubleShot | Laser
                  deriving (Eq)
-- WeaponType NormalShot | Missile | DoubleShot | Laser ... represents function of weapon that player selected, while
-- GameObject StandardRailgun | StandardLaser ... represents the object that is actually shot and rendered.
-- for example;
-- shooting NormalShot :: WeaponType and DoubleShot :: WeaponType both result in StandardRailgun :: GameObject creation, and
-- shooting Laser :: WeaponType creates StandardLaser :: GameObject when player is operating VicViper, or RippleLaser :: GameObject when LordBritish ... etc.

data ScrollBehavior = Enclosed{doesScroll :: Bool} | NoRollOut{doesScroll :: Bool}
 | RollOutAuto{doesScroll :: Bool, range :: Double}  | RollOutFold{doesScroll :: Bool}

-----------------------------
--
--   initialization
--
------------------------------
initialMonadius :: GameVariables -> Monadius
initialMonadius initVs = Monadius (initVs,initGameObjects)
    where
      initGameObjects = stars ++ [freshVicViper,freshPowerUpGauge]
      stars = take 26 $ map (\(t,i) -> Star{tag=Nothing,position = (fix 320 t:+fix 201 t),particleColor=colors!!i}) $ zip (map (\x -> square x + x + 41) [2346,19091..]) [1..]
      fix :: Int -> Int -> Double
      fix limit value = intToDouble $ (value `mod` (2*limit) - limit)
      colors = [Color3 1 1 1,Color3 1 1 0,Color3 1 0 0, Color3 0 1 1] ++ colors

{-
Default settings of game objects and constants
-}
downButton,leftButton,missileButton,powerUpButton,rightButton,selfDestructButton,shotButton,upButton :: Key
downButton = SpecialKey KeyDown
leftButton = SpecialKey KeyLeft
missileButton = Char 'x'
powerUpButton = Char 'c'
rightButton = SpecialKey KeyRight
selfDestructButton = Char 'g'
shotButton = Char 'z'
upButton = SpecialKey KeyUp


landScapeSensitive :: GameObject -> Bool
landScapeSensitive StandardRailgun{} = True -- these objects has hitDispLand
landScapeSensitive StandardLaser{} = True   -- in addition to hitDisp
landScapeSensitive Shield{} = True
landScapeSensitive _ = False

-----------------------------
--
--  game progress
--
-----------------------------

updateMonadius :: [Key] -> Monadius -> Monadius
updateMonadius realKeys (Monadius (variables,objects))
 = Monadius (newVariables,newObjects) where
  gameVariables = variables
  gameObjects   = objects
  gameLevel = baseGameLevel gameVariables
  bacterianShotSpeed = bacterianShotSpeedList!!gameLevel


  keys = if hp vicViper<=0 then [] else realKeys
   -- almost all operation dies when vicViper dies. use realKeys to fetch unaffected keystates.

  (newNextTag,newObjects) = issueTag (nextTag variables) $
                            ((loadObjects clock isEasy vicViper gameObjects) ++) $
                            filterJust.map scroll $
                            concatMap updateGameObject $
                            gameObjectsAfterCollision
  gameObjectsAfterCollision = collide objects
  -- * collision must be done BEFORE updateGameObject(moving), for
  --   players would like to see the moment of collision.
  -- * loading new objects after collision and moving is nice idea, since
  --   you then have new objects appear at exact place you wanted them to.
  -- * however, some operation would like to refer the result of the collision
  --   before it is actually taken effect in updateGameObject.
  --   such routine should use gameObjectsAfterCollision.


  newVariables = variables{
    nextTag = newNextTag,
    flagGameover = flagGameover variables ||  ageAfterDeath vicViper > 240,
    gameClock = (\c -> if hp vicViper<=0 then c else if goNextStage then 0 else c+1) $ gameClock variables,
    baseGameLevel = (\l -> if goNextStage then l+1 else l) $ baseGameLevel variables,
    totalScore = newScore,
    hiScore = max (hiScore variables) newScore
  }
   where
    goNextStage = gameClock variables > stageClearTime
    newScore = totalScore variables +  (sum) (map (\obj -> case obj of
      ScoreFragment{score = p} -> p
      _ -> 0) objects  :: [Int])

  updateGameObject :: GameObject -> [GameObject]
  -- update each of the objects and returns list of resulting objects.
  -- the list usually includes the modified object itself,
  --   may include several generated objects such as bullets and explosions,
  --   or include nothing if the object has vanished.

  updateGameObject vic@VicViper{} = newShields ++ makeMetalionShots vic{
    position=position vic + (vmag*(speed vic):+0) * (vx:+vy) ,
    trail=(if isMoving then ((position vic-(10:+0)):) else id) $ trail vic,
    powerUpLevels =
      (modifyArray gaugeOfShield (const (if (shieldCount > 0) then 1 else 0))) $
      (if doesPowerUp then (
      modifyArray (powerUpPointer vic)
        (\x -> if x<powerUpLimits!!powerUpPointer vic then x+1 else 0) . -- overpowering-up results in initial powerup level.
      (if powerUpPointer vic==gaugeOfDouble then modifyArray gaugeOfLaser (const 0) else id) . -- laser and double are
      (if powerUpPointer vic==gaugeOfLaser then modifyArray gaugeOfDouble (const 0) else id) ) --  exclusive equippment.
        else id) (powerUpLevels vic),
    powerUpPointer = if doesPowerUp then (-1) else powerUpPointer vic,
    speed = speeds !! (powerUpLevels vic!0),
    reloadTime = max 0 $ reloadTime vic - 1,
    ageAfterDeath = if hp vic>0 then 0
                    else ageAfterDeath vic+1,
    hitDisp = if treasure!!gameLevel then Circular 0 0 else Circular (0:+0) vicViperSize,
    hp = if selfDestructButton `elem` keys then 0 else hp vic
  } where
    isKey k = if k `elem` keys then 1 else 0
    vx = isKey rightButton + (-(isKey leftButton))
    vy = isKey upButton + (-(isKey downButton))
    vmag = if vx*vx+vy*vy>1.1 then sqrt(0.5) else 1
    isMoving = any (\b ->  elem b keys) [rightButton,leftButton,upButton,downButton]
    doesPowerUp = (powerUpButton `elem` keys) && (powerUpPointer vic >=0) &&
      (powerUpPointer vic ==0 ||
       powerUpLevels vic!powerUpPointer vic<powerUpLimits!!powerUpPointer vic)
    speeds = [2,4,6,8,11,14] ++ speeds
    shieldCount :: Int
    shieldCount = sum $ map (\o -> case o of
      Shield{} -> 1
      _ -> 0) gameObjects
    newShields = if (doesPowerUp && powerUpPointer vic==gaugeOfShield) then
      [freshShield{position=350:+260   ,placement=40:+shieldPlacementMargin,   angle=30,omega=10   },
       freshShield{position=350:+(-260),placement=40:+(-shieldPlacementMargin),angle=0 ,omega=(-10)}]
      else []

  updateGameObject option@Option{} = makeMetalionShots option{
    position = trail vicViper !! (10*optionTag option),
    reloadTime = max 0 $ reloadTime option - 1
  }

  updateGameObject miso@StandardMissile{} = [
                                       miso{position=newpos,
                                            mode = newmode,
                                            velocity=v,
                                            probe = (probe miso){position=newpos,hp=1}
                                           }
                                      ]  where
      newmode = if hp(probe miso) <= 0 then 1 else
        if mode miso == 0 then 0 else 2
      v = case newmode of
        0 -> 3.5:+(-7)
        1 -> 8:+0
        2 -> 0:+(-8)
        _ -> 0
      newpos = position miso + v

  updateGameObject shot@StandardRailgun{} = if hp shot <=0 then [] else
    [shot{position=position shot + velocity shot}]

  updateGameObject laser@StandardLaser{} = if hp laser <=0 then [] else
    [laser{position=(\(x:+_) -> x:+parentY) $ position laser + velocity laser,age=age laser+1}]  where
      myParent = head $ filter (\o -> tag o==Just (parentTag laser)) gameObjects
      _:+parentY = position myParent

  updateGameObject shield@Shield{} = if(hp shield<=0) then [] else [
    (if settled shield then
      shield{
        position=target,size=shieldPlacementMargin+intToDouble (hp shield),
        hitDisp = Circular (0:+0) (size shield+shieldHitMargin),
        hitDispLand = Circular (0:+0) (size shield)
      }
    else
      shield{hp=shieldMaxHp,size=5+intToDouble (hp shield),position=newPosition,settled=chaseFactor>0.6})
    {angle=angle shield + omega shield}
    ] where
      newPosition = position shield + v
      v =  difference * (chaseFactor:+0)
      chaseFactor = (10/magnitude difference)
      difference = target-position shield
      target = position vicViper+(realPart (placement shield) :+ additionalPlacementY)
      additionalPlacementY = signum (imagPart$placement shield)*size shield

  updateGameObject pow@PowerUpCapsule{} = if(hp pow<=0) then [freshScore 800] else [
    pow{age=age pow + 1}
    ] ++ if powerUpCapsuleHitBack!!gameLevel && age pow ==1 then
     map (\theta -> freshDiamondBomb{position=position pow,velocity=mkPolar (bacterianShotSpeed*0.5) theta}) $
       take 8 $ iterate (+(2*pi/8)) (pi/8)  else []


  updateGameObject bullet@DiamondBomb{} = if hp bullet<=0 then [] else
    [bullet{position=position bullet + velocity bullet,age=age bullet+1}]
  updateGameObject self@TurnGear{position=pos@(x:+y),mode=m} = if hp self<=0 then
     [freshScore 50] ++ freshExplosions pos ++ if turnGearHitBack!!gameLevel then [scatteredNeraiDan pos (bacterianShotSpeed:+0)] else []
   else [
    self{
      position = position self + velocity self,
      age = age self + 1,
      mode = newmode,
      velocity = newv
    }] where
      newv = case m of
        0 -> ((-4):+0)
        1 -> if (y - (imagPart.position) vicViper) > 0 then (3:+(-5)) else (3:+(5))
        _ -> if isEasy then 2:+0 else 6:+0
      newmode = if m==0 && x < (if not isEasy then -280 else 0) && (realPart.position) vicViper> (-270) then 1 else
                    if m==1 && abs (y - (imagPart.position) vicViper) < 20 then 2 else m

  updateGameObject me@SquadManager{position=pos,interval=intv,members=membs,age=clock,tag=Just myTag} =
    if mySquadIsWipedOut then(
      if currentScore me >= bonusScore me then map (\o -> o{position=pos}) (items me) else []
     )else me{
      age = age me + 1,
      currentScore = currentScore me + todaysDeaths,
      position = if clock <= releaseTimeOfLastMember then pos else warFront
    }:dispatchedObjects where
      dispatchedObjects = if (clock `div` intv < length membs && clock `mod` intv == 0) then
        [(membs!!(clock `div` intv)){position=pos,managerTag=myTag}] else []
      todaysDeaths = sum $ map (\o -> if hp o <=0 then 1 else 0) $ mySquad
      mySquadIsWipedOut = clock > releaseTimeOfLastMember && length mySquad <= 0
      warFront = position $ head $ mySquad
      releaseTimeOfLastMember = intv * (length membs-1)
      mySquad = filter (\o -> case o of
        TurnGear{managerTag=hisManagerTag} -> hisManagerTag == myTag  -- bad, absolutely bad code
        _                                  -> False) gameObjectsAfterCollision

  updateGameObject this@Flyer{position=pos@(x:+_),age=myAge,mode=m,velocity =v} =
    if gameClock variables > stageClearTime - 100 then freshExplosions pos else
    if hp this <=0 then ([freshScore (if mode this == 10 then 30 else 110)] ++ freshExplosions pos++(if hasItem this then [freshPowerUpCapsule{position=pos}] else if flyerHitBack!!gameLevel then [scatteredNeraiDan pos (bacterianShotSpeed:+0)] else []))
     else
    [this{
      age=myAge+1,
      position = pos + v ,
      velocity = newV,
      mode = newMode
    }]++myShots where
      newV = case m of
        00 -> (realPart v):+sin(intToDouble myAge / 5)
        01 -> v --if magnitude v <= 0.01 then if imagPart (position vicViper-pos)>0 then 0:+10 else 0:+(-10) else v
        02 -> (-4):+0
        10 -> if (not isEasy) || myAge < 10 then stokeV else v
        _ -> v
      stokeV = angleAccuracy 16 $ (* ((min (speed vicViper*0.75) (intToDouble$round$magnitude v)):+0) ) $ unitVector $ position vicViper-pos
      newMode = case m of
        01 -> if myAge > 20 && (position vicViper - pos) `innerProduct` v < 0 then 02 else 01
        _  -> m
      myShots = if (myAge+13*(fromJust $tag this)) `mod` myInterval == 0 && (x <= (-80) || x <= realPart(position vicViper))
        then [jikiNeraiDan pos (bacterianShotSpeed:+0)] else []
      myInterval = if m==00 || m==03 then flyerShotInterval!!gameLevel else inceptorShotInterval!!gameLevel

  updateGameObject me@Ducker{position=pos@(_:+_),velocity = v,age=myAge,gVelocity= vgrav,touchedLand = touched} =
    if hp me <=0 then ([freshScore 130] ++ freshExplosions pos ++ if hasItem me then [freshPowerUpCapsule{position=pos}] else[]) else
    [me{
      age=myAge+1,
      position = pos + v,
      charge = if charge me <=0 && aimRate > 0.9 && aimRate < 1.1 then (duckerShotCount!!gameLevel)*7+3
        else ((\x -> if x>0 then x-1 else x)  $  charge me),
      vgun = unitVector $ aimX:+aimY,
      velocity = if charge me >0 then 0:+0 else
        if magnitude v <= 0.01 then (
          if realPart(position vicViper - pos)>0 then 3:+0 else (-3):+0
        ) else if touched then
          ((realPart v):+(-imagPart vgrav))
        else (realPart v:+(imagPart vgrav)),
      touchedLand = False
    }]++myShots where
      aimX:+aimY = position vicViper - pos
      aimRate = (-(signum$realPart v))*aimX / (abs(aimY) +0.1)
      myShots = if charge me `mod` 7 /= 6 then [] else
        map (\w -> freshDiamondBomb{position=pos,velocity=w}) vs
      vs = map (\vy -> (vgun me)*(bacterianShotSpeed:+(1.5*(vy)))) [-duckerShotWay!!gameLevel+1 , -duckerShotWay!!gameLevel+3 .. duckerShotWay!!gameLevel-0.9]

  updateGameObject me@Jumper{position=pos@(_:+_),velocity = v,age=_,gravity= g,touchedLand = touched} =
    if hp me <=0 then ([freshScore 300] ++ freshExplosions pos ++if hasItem me then [freshPowerUpCapsule{position=pos}] else[]) else
    [
      me{
        position = pos + v,
        velocity = if touched then (signum(realPart $ position vicViper-pos)*abs(realPart v):+imagPart(jumpSize*g)) else v + g,
        jumpCounter = (if touched && v`innerProduct` g >0 then (+1) else id) $
          (if doesShot then (+1) else id) $ jumpCounter me,
        touchedLand = False
      }
    ] ++shots where
    jumpSize = if jumpCounter me `mod` 4 == 2 then (-30) else (-20)
    doesShot = jumpCounter me `mod` 4 == 2 && v`innerProduct` g >0
    shots = if doesShot then
      map (\theta -> freshDiamondBomb{position=pos,velocity=mkPolar (bacterianShotSpeed*jumperShotFactor!!gameLevel) theta}) $
        take way $ iterate (+(2*pi/intToDouble way)) 0
      else []
    way=jumperShotWay!!gameLevel

  updateGameObject me@ScrambleHatch{position = pos,age=a} =
    if hp me <=0 then [freshScore 3000] ++ freshMiddleExplosions pos ++
      if scrambleHatchHitBack!!gameLevel then hatchHitBacks else []
     else
    [me{
      age = a + 1,
      gateAngle = max 0$ min pi$ (if length currentLaunches>0 then (+1) else (+(-0.05))) $ gateAngle me
    }] ++ currentLaunches where
      currentLaunches = if a <= scrambleHatchLaunchLimitAge!!gameLevel then
          (map (\obj -> obj{position = pos}) $ launchProgram me!!a)
        else []
      hatchHitBacks =
        (map (\theta -> freshDiamondBomb{position=pos-16*gravity me,velocity=mkPolar (bacterianShotSpeed*0.5) theta}) $ take way $ iterate (+2*pi/intToDouble way) 0 )++
        (map (\theta -> freshDiamondBomb{position=pos-16*gravity me,velocity=mkPolar (bacterianShotSpeed*0.4) theta}) $ take way $ iterate (+2*pi/intToDouble way) (pi/intToDouble way) )
      way = 16


  updateGameObject me@Grashia{position = pos} =
    if hp me <=0 then ([freshScore 150] ++ freshExplosions pos++ if hasItem me then [freshPowerUpCapsule{position=pos}] else[]) else
    [
      me{
        age=age me+1,
        gunVector = unitVector $ position vicViper - pos,
        position = position me + ((-3)*sin(intToDouble (age me*mode me)/8):+0)
      } --V no shotto wo osoku
    ] ++ if age me `mod` myInterval == 0 && age me `mod` 200 > grashiaShotHalt!!gameLevel then
      [jikiNeraiDanAc (pos+gunVector me*(16:+0)) (grashiaShotSpeedFactor!!gameLevel*bacterianShotSpeed:+0) 64] else [] where
      myInterval = if mode me == 0 then grashiaShotInterval!!gameLevel else landRollShotInterval!!gameLevel

  updateGameObject me@Particle{position = pos} =
    if age me > expireAge me then (if particleHitBack!!gameLevel then [freshScore 10,scatteredNeraiDan pos (bacterianShotSpeed:+0)] else []) else
    [me{
      age = age me + 1,
      position = position me + (decay:+0) * velocity me
    }] where
      decay =  exp $  -  intToDouble (age me) / decayTime me

  updateGameObject me@LandScapeBlock{position = pos,velocity = v} = [me{position = pos+v}]

  updateGameObject DebugMessage{} = []

  updateGameObject ScoreFragment{} = []

  updateGameObject me@SabbathicAgent{fever = f} = if gameClock variables>stageClearTime-180  then [] else [
    me{
      fever = if launch then f+1 else f
    }]
    ++ if launch then map (\pos -> freshStalk{position = pos,velocity=(-4):+0,hasItem = (realPart pos>0 && (round $ imagPart pos :: Int) `mod` (3*round margin)==0)}) $
    concat $ map (\t -> [(340:+t),((-340):+t),(t:+(260)),(t:+(-260))]) $[(-margin*df),(((negate margin) * df) + (margin * 2))..(margin*df+1)] else [] where
      launch = (<=0) $ length $ filter (\obj -> case obj of
        Flyer{} -> True
        _ -> False) objects
      df = intToDouble f - 1
      margin :: Double
      margin = 20

  updateGameObject x = [x]

  makeMetalionShots :: GameObject -> [GameObject]
  {- this generates proper playerside bullets
  according to the current power up state of vicviper.
  both options and vicviper is updated using this. -}

  makeMetalionShots obj = obj{reloadTime=reloadTime obj+penalty1+penalty2,
                              weaponEnergy = max 0 $ min 100 $ weaponEnergy obj + if doesLaser then (-10) else 50 }
                                      :(shots ++ missiles) where
    (shots,penalty1) = if doesNormal then ([freshStandardRailgun{position=position obj,parentTag=myTag}] ,2)
      else if doesDouble then ([freshStandardRailgun{position=position obj,parentTag=myTag},freshStandardRailgun{position=position obj,parentTag=myTag,velocity=mkPolar 1 (pi/4)*velocity freshStandardRailgun}] ,2)
      else if doesLaser then ([freshStandardLaser{position=position obj+(shotSpeed/2:+0),parentTag=myTag}] ,1)
      else ([],0)
    penalty2 = if weaponEnergy obj <= 0 then 8 else 0
    missiles = if doesMissile then [freshStandardMissile{position=position obj}] else []
    doesShot = (isJust $tag obj) && (reloadTime obj <=0) && (shotButton `elem` keys)
    doesNormal = doesShot && elem NormalShot types && (shotCount<2)
    doesDouble = doesShot && elem DoubleShot types && (shotCount<1)
    doesLaser = doesShot && elem Laser types
    doesMissile = (isJust $tag obj) && elem Missile types && (missileButton `elem` keys) && (missileCount<=0)
    myTag = fromJust $ tag obj
    shotCount = length $ filter (\o -> case o of
                            StandardRailgun{} -> parentTag o==myTag
                            _             -> False) gameObjects
    missileCount = length $ filter (\o -> case o of
                                           StandardMissile{} -> True
                                           _                 -> False) gameObjects

    types = weaponTypes vicViper

  jikiNeraiDan :: Complex Double -> Complex Double -> GameObject
  -- an enemy bullet starting at position sourcePos and with relative velocity initVelocity.
  -- bullet goes straight to vicviper if initVelocity is a positive real number.
  jikiNeraiDanAc sourcePos initVelocity accuracy = freshDiamondBomb{
    position = sourcePos,
    velocity = (*initVelocity) $ (angleAccuracy accuracy) $ unitVector $ position vicViper - sourcePos
  }
  jikiNeraiDan sourcePos initVelocity = jikiNeraiDanAc sourcePos initVelocity 32

  scatteredNeraiDan :: Complex Double -> Complex Double -> GameObject
  -- a rather scattered jikiNeraiDan.
  scatteredNeraiDan sourcePos initVelocity = freshDiamondBomb{
    position = sourcePos,
    velocity = scatter $ (*initVelocity) $ (angleAccuracy 32) $ unitVector $ position vicViper - sourcePos
  } where
    scatter z = let (r,theta)=polar z in
      mkPolar r (theta+pi/8*((^(3::Int)).sin)((intToDouble $ gameClock variables) + magnitude sourcePos))

  freshExplosionParticle pos vel a = Particle{tag=Nothing,position=pos,velocity=vel,size=8,particleColor=Color3 1 0.5 0,age=a,decayTime=6,expireAge=20}

  freshExplosions pos = take 5 expls where
    expls :: [GameObject]
    expls = makeExp randoms
    randoms = [square $ sin(9801*sqrt t*(intToDouble$gameClock variables) + magnitude pos)|t<-[1..]]
    makeExp (a:b:c:xs) = (freshExplosionParticle pos (mkPolar (3*a) (2*pi*b)) (round $ -5*c)):makeExp xs
    makeExp _ = []

  freshMiddleExplosions pos = take 16 expls where
    expls :: [GameObject]
    expls = makeExp randoms 0
    randoms = [square $ sin(8086*sqrt t*(intToDouble$gameClock variables) + magnitude pos)|t<-[1..]]
    makeExp (a:b:xs) i = (freshExplosionParticle (pos+mkPolar 5 (pi/8*i)) (mkPolar (6+3*a) (pi/8*i)) (round $ -5*b)){size=16}:makeExp xs (i+1)
    makeExp _ _ = []

  -- issue tag so that each objcet has unique tag,
  -- and every object will continue to hold the same tag.
  issueTag :: Int -> [GameObject] -> (Int,[GameObject])
  issueTag nt [] = (nt,[])
  issueTag nt (x:xs) = (newNextTag,taggedX:taggedXs)
      where
        (nextTagForXs,taggedX) = if(isNothing $ tag x) then (nt+1,x{tag = Just nt}) else (nt,x)
        (newNextTag,taggedXs)  = issueTag nextTagForXs xs

  collide :: [GameObject] -> [GameObject]
  -- collide a list of GameObjects and return the result.
  -- it is important NOT to delete any object at the collision -- collide, show then delete
  collide = map personalCollide
    where
    -- each object has its own hitClasses and weakPoints.
    -- collision is not symmetric: A may crushed by B while B doesn't feel A.
    -- object X is hit by only objectsWhoseHitClassIsMyWeakPoint X.
    personalCollide :: GameObject -> GameObject
    personalCollide obj = foldr check obj $ objectsWhoseHitClassIsMyWeakPoint obj

    objectsWhoseHitClassIsMyWeakPoint :: GameObject -> [GameObject]
    objectsWhoseHitClassIsMyWeakPoint me =
      filter (\him -> not $ null $ (weakPoint me) `intersect` (hitClass him)) gameObjects

    hitClass :: GameObject -> [HitClass]
    hitClass VicViper{} = [MetalionBody,ItemReceiver]
    hitClass StandardMissile{} = [MetalionShot]
    hitClass StandardRailgun{} = [MetalionShot]
    hitClass StandardLaser{} = [MetalionShot]
    hitClass Shield{} = [MetalionBody]
    hitClass PowerUpCapsule{} = [PowerUp]

    hitClass DiamondBomb{} = [BacterianShot]
    hitClass TurnGear{} = [BacterianBody]
    hitClass Flyer{} = [BacterianBody]
    hitClass Ducker{} = [BacterianBody]
    hitClass Jumper{} = [BacterianBody]
    hitClass Grashia{} = [BacterianBody]
    hitClass ScrambleHatch{} = [BacterianBody,LaserAbsorber]

    hitClass LandScapeBlock{} = [LandScape]
    hitClass _ = []

    weakPoint :: GameObject -> [HitClass]
    weakPoint VicViper{} = [PowerUp,BacterianBody,BacterianShot,LandScape]
    weakPoint StandardMissile{} = [BacterianBody,LandScape]
    weakPoint Probe{} = [LandScape]
    weakPoint StandardRailgun{} = [BacterianBody,LandScape]
    weakPoint StandardLaser{} = [LaserAbsorber,LandScape]
    weakPoint Shield{} = [BacterianBody,BacterianShot,LandScape]
    weakPoint PowerUpCapsule{} = [ItemReceiver]

    weakPoint DiamondBomb{} = [MetalionBody,LandScape]
    weakPoint TurnGear{} = [MetalionBody,MetalionShot]
    weakPoint Flyer{} = [MetalionBody,MetalionShot]
    weakPoint Ducker{} = [MetalionBody,MetalionShot,LandScape]
    weakPoint Jumper{} = [MetalionBody,MetalionShot,LandScape]
    weakPoint Grashia{} = [MetalionBody,MetalionShot]
    weakPoint ScrambleHatch{} = [MetalionBody,MetalionShot]

    weakPoint _ = []

    -- after matching hitClass-weakPoint, you must check the shape of the pair of object
    -- to see if source really hits the target.
    check :: GameObject -> GameObject -> GameObject
    check source target = case (source, target) of
      (LandScapeBlock{},StandardMissile{}) -> if(hit source target) then (affect source target2) else target2 where
        target2 = target{probe = if(hit source p) then (affect source p) else p}
        p = probe target
      _                   -> if(hit source target) then (affect source target) else target

    -- if a is really hitting b, a affects b (usually, decreases hitpoint of b).
    -- note that landScapeSensitive objects have special hitDispLand other than hitDisp.
    -- this allows some weapons to go through narrow land features, and yet
    -- wipe out wider area of enemies.
    hit :: GameObject -> GameObject -> Bool
    hit a b = case (a,b) of
      (LandScapeBlock{},c) -> if landScapeSensitive c then (position a +> hitDisp a) >?< (position c +> hitDispLand c)
         else (position a +> hitDisp a) >?< (position b +> hitDisp b)
      _ -> (position a +> hitDisp a) >?< (position b +> hitDisp b)

    affect :: GameObject -> GameObject -> GameObject
    affect VicViper{} obj = case obj of
      pow@PowerUpCapsule{} -> pow{hp = hp pow-1}
      x                    -> x
    affect PowerUpCapsule{} obj = case obj of
      viper@VicViper{} -> viper{powerUpPointer = (\x -> if x >=5 then 0 else x+1)$powerUpPointer viper}
      _                -> error "Power capsule should not have been able to affect anything but the player craft."
    affect StandardMissile{} obj = obj{hp = hp obj-(hatchHP`div`2 + 2)} -- 2 missiles can destroy a hatch
    affect StandardRailgun{} obj = obj{hp = hp obj-(hatchHP`div`4 + 1)} -- 4 shots can also destroy a hatch
    affect StandardLaser{} obj = obj{hp = hp obj-1}
    affect Shield{} obj = obj{hp = hp obj-1}


    affect DiamondBomb{} obj = obj{hp = hp obj-1}
    affect TurnGear{} obj = obj{hp = hp obj-1}
    affect Flyer{} obj = obj{hp = hp obj-1}
    affect Ducker{} obj= obj{hp = hp obj-1}
    affect Jumper{} obj= obj{hp = hp obj-1}
    affect Grashia{} obj= obj{hp = hp obj-1}
    affect ScrambleHatch{} obj= obj{hp = hp obj-1}

    affect LandScapeBlock{} obj = case obj of
--      miso@StandardMissile{velocity=v} -> miso{velocity = (1:+0)*abs v}
      duck@Ducker{} -> duck{touchedLand=True}
      that@Jumper{} -> that{touchedLand=True}
      _ -> obj{hp = hp obj-1}

    affect _ t = t

  scroll :: GameObject -> Maybe GameObject
  -- make an object scroll.
  -- if the object is to vanish out of the screen, it becomes Nothing.
  scroll obj = let
      (x:+y) = position obj
      scrollBehavior :: GameObject -> ScrollBehavior
      scrollBehavior VicViper{} = Enclosed False
      scrollBehavior Option {}  = NoRollOut False
      -- We use the more verbose way of setting records here to guarantee
      -- 'range' is needed so -Wall doesn't get fooled.
      scrollBehavior StandardRailgun{} = RollOutAuto {doesScroll = True, range = shotSpeed}
      scrollBehavior StandardLaser{} = RollOutAuto True laserSpeed
      scrollBehavior PowerUpGauge{} = NoRollOut False
      scrollBehavior PowerUpCapsule{} = RollOutAuto True 40
      scrollBehavior Shield{} = NoRollOut False

      scrollBehavior DiamondBomb{} = RollOutAuto False 10
      scrollBehavior TurnGear{} = RollOutAuto False 20
      scrollBehavior SquadManager{} = NoRollOut False
      scrollBehavior ScrambleHatch{} = RollOutAuto True 60

      scrollBehavior LandScapeBlock{} = RollOutAuto True 160

      scrollBehavior Star{} = RollOutFold True

      scrollBehavior DebugMessage{} = NoRollOut False
      scrollBehavior ScoreFragment{} = NoRollOut False
      scrollBehavior SabbathicAgent{} = NoRollOut False

      scrollBehavior _          = RollOutAuto True 40

      scrollSpeed = if hp vicViper <= 0 then 0 else if gameClock variables <=6400 then 1 else 2
      rolledObj = if doesScroll $ scrollBehavior obj then obj{position=(x-scrollSpeed):+y} else obj
    in case scrollBehavior obj of
      Enclosed _ -> Just rolledObj{position = (max (-300) $ min 280 x):+(max (-230) $ min 230 y)}
      NoRollOut _ -> Just rolledObj
      RollOutAuto _ r -> if any (>r) [x-320,(-320)-x,y-240,(-240)-y] then Nothing
                        else Just rolledObj
      RollOutFold _ -> Just rolledObj{position = (if x< -320 then x+640 else x):+y} where
        (_:+_) = position rolledObj

  clock = gameClock variables
  isEasy = gameLevel <= 1

  vicViper = fromJust $ find (\obj -> case obj of
                            VicViper{} -> True
                            _          -> False) objects

-- things needed both for progress and rendering
weaponTypes :: GameObject -> [WeaponType]
weaponTypes viper@VicViper{}
  | check gaugeOfDouble = DoubleShot : missile
  | check gaugeOfLaser  = Laser      : missile
  | otherwise           = NormalShot : missile
  where
    check g = powerUpLevels viper!g>0
    missile = if check gaugeOfMissile then [Missile] else []
weaponTypes _ = []

isMonadiusOver :: Monadius -> Bool
isMonadiusOver (Monadius (vars,_)) = flagGameover vars
