module Scene.Opening
  ( scene
  , OpenState(..)
  ) where

import Control.Monad (mplus)
import Prelude hiding (catch)

import Data.IORef
import Data.Complex -- ((:+), Complex())

import Demo -- (ReplayInfo(), demoData)
import Monadius
import Recorder
import Util (intToDouble)

import GLWrapper hiding (color)
import qualified GLWrapper as GLW (color)

import GlobalVariables

presentationMode :: Bool
presentationMode = True

type OpenVars = (Int,Int)

data OpenState
  = Opening OpenVars GlobalVariables
  | Main GlobalVariables (IORef Recorder)

------------------------------------------------------------
-- utility
-- 

color :: Double -> Double -> Double -> IO ()
color a b c = GLW.color $ Color3 a b c

render :: Vector2 Double -> (Double,Double,Double) -> IO () -> IO ()
render trans (x,y,z) proc = preservingMatrix $ do
  let (Vector2 a b) = trans
  translate (Vector3 a b 0)
  scale x y z
  proc

timeLimit :: Int
timeLimit = 30

-- renderStringGrad :: (forall a. (Font a)) => Int -> a -> Int -> String -> IO ()
renderStringGrad clock font delay str = renderString font (take (((clock-delay) * length str) `div` timeLimit) str)

addVX, addVY :: Vector2 Double -> Double -> Vector2 Double
addVX (Vector2 x y) x' = Vector2 (x+x') y
addVY (Vector2 x y) y' = Vector2 x (y+y')

------------------------------------------------------------
-- specialized render functions
-- 

--------------------
-- renderLogo
renderLogo :: Int -> IO ()
renderLogo clock = do
  let col (a,b,c) = color a b c
      defColor = (0,0.2,0.8)
      (a,b,c) +@ x = (a+x, b+x, c+x)

  if clock < drawCompleteTime
    then col defColor
    else col (defColor +@ shine clock)
  render (Vector2 0 120) (1.05,1,1)
    $ mapM_ (renderPrimitive LineStrip . renderVertices2D.delayVertices clock) [lambdaLfoot,lambdaRfoot]

  cWhite
  render (Vector2 (-195) 130) (0.73, 0.56, 0.56) $ renderStringGrad clock Roman 0 "Monadius"
  where
     shine t = monoshine (drawCompleteTime + t) + monoshine (drawCompleteTime + t+6)
     monoshine t = exp(-0.2*intToDouble(t`mod` 240))
     drawCompleteTime = length lambdaRfoot
     lambdaLfoot = moreVertices $ [10:+55,(-15):+0] ++ map (\(x:+y)->((-x):+y)) wing
     lambdaRfoot = moreVertices $ [(-15):+70,(-12):+77,(-5):+80,(2:+77),(5:+70)] ++ wing
     wing = [(30:+0),(200:+0),(216:+16),(208:+24),(224:+24),(240:+40),(232:+48),(248:+48),(272:+72),(168:+72)]
     delayVertices clck = reverse . take clck . reverse

     moreVertices (a:b:cs)
       | magnitude (a-b) > d = moreVertices (a:((a+b)/(2:+0)):b:cs) 
       | otherwise = a:moreVertices(b:cs)
       where d=6
     moreVertices x = x

     renderVertices2D xys = mapM_ (\(x:+y) -> vertex $ Vertex2 x y) xys

--------------------
-- renderMenu


renderMenu :: Int -> Int -> (Int,Int) -> IO ()
renderMenu clock menuCursor (savedLevel,savedArea) = do
  let r = renderStringGrad clock Roman 60
  let v = Vector2 (-230) (-200)
  select $ menuCursor==0
  render v (0.2, 0.2, 0.3)
    $ r $ (cursor $ menuCursor==0) ++ "New Game"
  select $ menuCursor==1  
  render (addVX v 300) (0.2, 0.2, 0.3)
    $ r $ (cursor $ menuCursor==1) ++ "Continue " ++ (show savedLevel) ++ "-" ++ (show $ (+1) savedArea)
  where
    cursor True = ">"
    cursor False = " "
    select True = cYellow
    select False = cWhite

cWhite, cYellow :: IO ()
cWhite  = color 1.0 1.0 1.0
cYellow = color 1.0 1.0 0
--------------------
-- renderSubText
renderSubText :: Int -> IO ()
renderSubText clock = do
  let r = renderStringGrad clock Roman
  cWhite
  let v = (Vector2 (-250) 75)
  render v (0.15,0.10,0.15) $ r 10 "Dedicated to the makers, the players, the history,"
  render (v `addVY` (-20)) (0.15,0.10,0.15) $ r 20 "  and the 20th anniversary of GRADIUS series."

--------------------
-- renderInstructions
renderInstructions :: Int -> IO ()
renderInstructions clock = do
  mapM_ (\ (y,(strA,strB),i) -> do
    let r = renderStringGrad clock Roman (20 + i*5)
    let v = (Vector2 (-180) y)
    render v             (0.18, 0.18, 0.2) $ r strA
    render (addVX v 240) (0.18, 0.18, 0.2) $ r strB
    ) $ zip3 [0,(-35)..] instructions [1..]
  where
    instructions = [("Move","Arrow Keys"),("Shot","Z Key"),("Missile","X Key"),("Power Up","C Key"),("Start","Space Bar")]

------------------------------------------------------------
-- openingProc
--

scene :: OpenVars -> GlobalVariables -> [Key] -> IO OpenState
scene (clock,menuCursor) vars keystate
  | recorderMode vars == Playback = gameStart Playback vars (playbackSaveState vars) (isCheat vars)
  | clock > demoStartTime = demoStart vars
  | otherwise = do
  initGraphics

  renderLogo clock
  renderMenu clock menuCursor (saveState vars)
  renderSubText clock
  renderInstructions clock

  swapBuffers

  let gameStart' = gameStart (recorderMode vars) vars
  if Char ' ' `elem` keystate && clock >= timeLimit
    then if menuCursor == 0
      then gameStart' (1,0) False
      else gameStart' (saveState vars) (isCheat vars)
    else case getNumberKey keystate of
      Just n  -> gameStart' (n,0) True
      Nothing -> return $ Opening (clock+1, nextCursor menuCursor keystate) vars
  where
    demoStartTime | presentationMode = 480
                  | otherwise = 1800

    demoStart vrs = do
      let i = demoIndex vrs
      let ReplayInfo (state,dat) = demoData!!i
      gameStart Playback vrs{
        playBackName = Just "Press Space",
        playbackKeys = decode dat,
        demoIndex = i + 1
      } state (isCheat vrs)

    getNumberKey ks = foldl mplus Nothing $ map keyToNumber ks

    nextCursor cursor keys
      | check KeyLeft  = 0
      | check KeyRight = 1
      | otherwise      = cursor
      where check k = SpecialKey k `elem` keys

    gameStart recordermode vrs (level,area) ischeat = do
      -- it is possible to temporary set (recordermode /= recorderMode vars)
      gs <- newIORef $ initialRecorder recordermode (playbackKeys vrs) (initialMonadius GameVariables{
      totalScore=0, flagGameover=False,  hiScore=saveHiScore vrs,
      nextTag=0, gameClock = savePoints!!area ,baseGameLevel = level,
      playTitle = if recordermode /= Playback then Nothing else playBackName vrs})
    
      return $ Main vrs{isCheat=ischeat,recordSaveState=(level,area)} gs

    keyToNumber :: Key -> Maybe Int
    keyToNumber (Char c) | c>='0' && c<='9' = Just $ fromEnum c - fromEnum '0'
    keyToNumber _ = Nothing

