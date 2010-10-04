module OpeningProc
  ( openingProc
  , OpenState(..)
  ) where

import Control.Monad (mplus)
import Prelude hiding (catch)

import Data.IORef
import Data.Complex -- ((:+), Complex())
import Data.Maybe (fromJust, isJust)

import Demo -- (ReplayInfo(), demoData)
import Monadius
import Recorder
import Util (intToDouble)

import GLWrapper hiding (color)
import qualified GLWrapper as GLW (color)

import GlobalVariables


presentationMode :: Bool
presentationMode = True

data OpenState
  = Opening Int Int GlobalVariables
  | Main GlobalVariables (IORef Recorder)

------------------------------------------------------------
-- utility
-- 

initGraphics :: IO ()
initGraphics = do
  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

color :: Double -> Double -> Double -> IO ()
color a b c = GLW.color $ Color3 a b c

render :: Vector3 Double -> (Double,Double,Double) -> IO () -> IO ()
render trans (x,y,z) proc = preservingMatrix $ do
    translate trans
    scale x y z
    proc

timeLimit :: Int
timeLimit = 30

-- renderStringGrad :: (forall a. (Font a)) => Int -> a -> Int -> String -> IO ()
renderStringGrad clock font delay str = renderString font (take (((clock-delay) * length str) `div` timeLimit) str)

------------------------------------------------------------
-- specialized render functions
-- 

--------------------
-- renderLogo
renderLogo :: Int -> IO ()
renderLogo clock = do
  let col (a,b,c) = color a b c
      defColor = (0,0.2,0.8)
      addScalarToTuple (a,b,c) x = (a+x, b+x, c+x)
  if clock < drawCompleteTime
    then col defColor
    else col (addScalarToTuple defColor (shine clock))
  render (Vector3 0 120 0) (1.05,1,1)
    $ mapM_ (renderPrimitive LineStrip . renderVertices2D.delayVertices clock) [lambdaLfoot,lambdaRfoot]

  color 1.0 1.0 1.0
  render (Vector3 (-195) 130 0) (0.73, 0.56, 0.56)
    $ renderStringGrad clock Roman 0 "Monadius"
  where
     shine t = monoshine (drawCompleteTime + t) + monoshine (drawCompleteTime + t+6)
     monoshine t = exp(-0.2*intToDouble(t`mod` 240))
     drawCompleteTime = length lambdaRfoot
     lambdaLfoot = moreVertices $ [10:+55,(-15):+0] ++ map (\(x:+y)->((-x):+y)) wing
     lambdaRfoot = moreVertices $ [(-15):+70,(-12):+77,(-5):+80,(2:+77),(5:+70)] ++ wing
     wing = [(30:+0),(200:+0),(216:+16),(208:+24),(224:+24),(240:+40),(232:+48),(248:+48),(272:+72),(168:+72)]
     delayVertices clck vs = (reverse . take clck . reverse) vs

     moreVertices (a:b:cs)
       | magnitude (a-b) > d = moreVertices (a:((a+b)/(2:+0)):b:cs) 
       | otherwise = a:moreVertices(b:cs)
       where d=6
     moreVertices x = x

     renderVertices2D :: [Complex Double] -> IO ()
     renderVertices2D xys = mapM_ (\(x:+y) -> vertex $ Vertex3 x y 0) xys

--------------------
-- renderMenu
renderMenu :: Int -> Int -> (Int,Int) -> IO ()
renderMenu clock menuCursor (savedLevel,savedArea) = do
  if menuCursor==0 then cYellow else cWhite
  render (Vector3 (-230) (-200) 0) (0.2, 0.2, 0.3)
    $ renderStringGrad clock Roman 60 $ (cursor $ menuCursor==0) ++ "New Game"
    
  if menuCursor==1 then cYellow else cWhite
  render (Vector3 70 (-200) 0) (0.2, 0.2, 0.3)
    $ renderStringGrad clock Roman 60 $ (cursor $ menuCursor==1) ++ "Continue " ++ (show savedLevel) ++ "-" ++ (show $ (+1) savedArea)
  where
    cYellow = color 1.0 1.0 0
    cWhite  = color 1.0 1.0 1.0
    cursor True = ">"
    cursor False = " "

--------------------
-- renderSubText
renderSubText :: Int -> IO ()
renderSubText clock = do
  color 1.0 1.0 1.0
  render (Vector3 (-250) 75 0) (0.15, 0.10, 0.15)
    $ renderStringGrad clock Roman 10 "Dedicated to the makers, the players, the history,"
  render (Vector3 (-250) 55 0) (0.15, 0.10, 0.15)
    $ renderStringGrad clock Roman  20 "  and the 20th anniversary of GRADIUS series."

--------------------
-- renderInstructions
renderInstructions :: Int -> IO ()
renderInstructions clock = do
  mapM_ (\ (y,(strA,strB),i) -> do
    render (Vector3 (-180) y 0) (0.18, 0.18, 0.2)
      $ renderStringGrad clock Roman (20 + i*5) strA
    render (Vector3 60 y 0) (0.18, 0.18, 0.2)
      $ renderStringGrad clock Roman (25 + i*5) strB
    ) $ zip3 [0,(-35)..] instructions [1..]
  where
    instructions = [("Move","Arrow Keys"),("Shot","Z Key"),("Missile","X Key"),("Power Up","C Key"),("Start","Space Bar")]

------------------------------------------------------------
-- openingProc
--

openingProc :: Int -> Int -> GlobalVariables -> [Key] -> IO OpenState
openingProc clock menuCursor vars keystate
  | recorderMode vars == Playback = gameStart (fst $ playbackSaveState vars) (snd $ playbackSaveState vars) (isCheat vars) Playback vars
  | clock > demoStartTime = demoStart vars
  | otherwise = do
  initGraphics

  renderLogo clock
  renderMenu clock menuCursor (saveState vars)
  renderSubText clock
  renderInstructions clock

  swapBuffers

  let (savedLevel,savedArea) = saveState vars
  if Char ' ' `elem` keystate && clock >= timeLimit then
     if menuCursor == 0 then
       gameStart 1 0 False (recorderMode vars) vars
     else
       gameStart savedLevel savedArea (isCheat vars) (recorderMode vars) vars
   else if isJust $ getNumberKey keystate then
      gameStart (fromJust $ getNumberKey keystate) 0 True (recorderMode vars) vars
    else return $ Opening (clock+1) (nextCursor menuCursor keystate) vars
  where
    demoStartTime = if presentationMode then 480 else 1800

    demoStart vrs = do
      let i = demoIndex vrs
      let ReplayInfo ((lv,area),dat) = demoData!!i
      gameStart lv area (isCheat vrs) Playback vrs{
        playBackName = Just "Press Space",
        playbackKeys = decode dat,
        demoIndex = demoIndex vrs+1
      }

    getNumberKey ks = foldl mplus Nothing $ map keyToNumber ks

    nextCursor menuCursor keys =
      if SpecialKey KeyLeft `elem` keys then 0 else
      if SpecialKey KeyRight `elem` keys then 1 else
      menuCursor

    gameStart level area ischeat recordermode vrs = do
      -- it is possible to temporary set (recordermode /= recorderMode vars)
      gs <- newIORef $ initialRecorder recordermode (playbackKeys vrs) (initialMonadius GameVariables{
      totalScore=0, flagGameover=False,  hiScore=saveHiScore vrs,
      nextTag=0, gameClock = savePoints!!area ,baseGameLevel = level,
      playTitle = if recordermode /= Playback then Nothing else playBackName vrs})
    
      return $ Main vrs{isCheat=ischeat,recordSaveState=(level,area)} gs

    keyToNumber :: Key -> Maybe Int
    keyToNumber k = case k of
      Char c -> if c>='0' && c<='9' then Just $ fromEnum c - fromEnum '0' else Nothing
      _      -> Nothing

