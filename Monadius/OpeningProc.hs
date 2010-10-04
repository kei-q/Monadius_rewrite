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

import GLWrapper

import GlobalVariables


presentationMode :: Bool
presentationMode = True

data OpenState
  = Opening Int Int GlobalVariables
  | Main GlobalVariables (IORef Recorder)

openingProc :: Int -> Int -> GlobalVariables -> [Key] -> IO OpenState
openingProc clock menuCursor vars keystate
  | recorderMode vars == Playback = gameStart (fst $ playbackSaveState vars) (snd $ playbackSaveState vars) (isCheat vars) Playback vars
  | clock > demoStartTime = demoStart vars
  | otherwise = do
  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  if clock < drawCompleteTime then color $ Color3 (0 :: Double) 0.2 0.8
    else color $ Color3 (0+shine clock :: Double) (0.2+shine clock) (0.8+shine clock)
  preservingMatrix $ do
    translate (Vector3 0 (120 :: Double) 0)
    scale 1.05 1 (1 :: Double)
    mapM_ (renderPrimitive LineStrip . renderVertices2D.delayVertices clock) [lambdaLfoot,lambdaRfoot]
  color $ Color3 (1.0 :: Double) 1.0 1.0
  preservingMatrix $ do
    translate $ Vector3 (-195 :: Double) (130) 0
    scale (0.73 :: Double) 0.56 0.56
    renderStringGrad Roman 0 "Monadius"
  preservingMatrix $ do
    if menuCursor==0 then color $ Color3 (1.0 :: Double) 1.0 0 else color $ Color3 (1.0 :: Double) 1.0 1.0
    translate $ Vector3 (-230 :: Double) (-200) 0
    scale (0.2 :: Double) 0.2 0.3
    renderStringGrad Roman 60 $ (if menuCursor==0 then ">" else " ") ++ "New Game"
  preservingMatrix $ do
    if menuCursor==1 then color $ Color3 (1.0 :: Double) 1.0 0 else color $ Color3 (1.0 :: Double) 1.0 1.0
    translate $ Vector3 (70 :: Double) (-200) 0
    scale (0.2 :: Double) 0.2 0.3
    renderStringGrad Roman 60 $ (if menuCursor==1 then ">" else " ") ++ "Continue " ++ (show . fst . saveState) vars++ "-" ++ (show . (+1) . snd . saveState) vars
  color $ Color3 (1.0 :: Double) 1.0 1.0

  preservingMatrix $ do
    translate $ Vector3 (-250 :: Double) (75) 0
    scale (0.15 :: Double) 0.10 0.15
    renderStringGrad Roman 10 "Dedicated to the makers, the players, the history,"
  preservingMatrix $ do
    translate $ Vector3 (-250 :: Double) (55) 0
    scale (0.15 :: Double) 0.10 0.15
    renderStringGrad Roman  20 "  and the 20th anniversary of GRADIUS series."
  mapM_ (\ (y,(strA,strB),i) -> preservingMatrix $ do
    preservingMatrix $ do
      translate $ Vector3 (-180 :: Double) y 0
      scale (0.18 :: Double) 0.18 0.2
      renderStringGrad Roman (20 + i*5) strA
    preservingMatrix $ do
      translate $ Vector3 (60 :: Double) y 0
      scale (0.18 :: Double) 0.18 0.2
      renderStringGrad Roman (25 + i*5) strB
    ) $ zip3 [0,(-35)..] instructions [1..]

  swapBuffers

  if Char ' ' `elem` keystate && clock >= timeLimit then
     if menuCursor == 0 then
       gameStart 1 0 False (recorderMode vars) vars
     else
       gameStart savedLevel savedArea (isCheat vars) (recorderMode vars) vars
   else if isJust $ getNumberKey keystate then
      gameStart (fromJust $ getNumberKey keystate) 0 True (recorderMode vars) vars
    else return $ Opening (clock+1) (nextCursor keystate) vars
  where
     instructions = [("Move","Arrow Keys"),("Shot","Z Key"),("Missile","X Key"),("Power Up","C Key"),("Start","Space Bar")]
     timeLimit = 30 :: Int
     renderStringGrad font delay str = renderString font (take (((clock-delay) * length str) `div` timeLimit) str)
     getNumberKey keystate = foldl mplus Nothing $ map keyToNumber keystate

     keyToNumber :: Key -> Maybe Int
     keyToNumber k = case k of
       Char c -> if c>='0' && c<='9' then Just $ fromEnum c - fromEnum '0' else Nothing
       _      -> Nothing

     gameStart level area ischeat recordermode vrs = do
       -- it is possible to temporary set (recordermode /= recorderMode vars)
       gs <- newIORef $ initialRecorder recordermode (playbackKeys vrs) (initialMonadius GameVariables{
       totalScore=0, flagGameover=False,  hiScore=saveHiScore vrs,
       nextTag=0, gameClock = savePoints!!area ,baseGameLevel = level,
       playTitle = if recordermode /= Playback then Nothing else playBackName vrs})

       return $ Main vrs{isCheat=ischeat,recordSaveState=(level,area)} gs

     (savedLevel,savedArea) = saveState vars

     demoStart vrs = do
       let i = demoIndex vrs
       let ReplayInfo ((lv,area),dat) = demoData!!i
       gameStart lv area (isCheat vrs) Playback vrs{
         playBackName = Just "Press Space",
         playbackKeys = decode dat,
         demoIndex = demoIndex vrs+1
       }

     nextCursor keys =
       if SpecialKey KeyLeft `elem` keys then 0 else
       if SpecialKey KeyRight `elem` keys then 1 else
       menuCursor

     delayVertices clck vs = (reverse . take clck . reverse) vs

     lambdaLfoot = moreVertices $ [10:+55,(-15):+0] ++ map (\(x:+y)->((-x):+y)) wing
     lambdaRfoot = moreVertices $ [(-15):+70,(-12):+77,(-5):+80,(2:+77),(5:+70)] ++ wing

     shine t = monoshine (drawCompleteTime + t) + monoshine (drawCompleteTime + t+6)

     monoshine t = exp(-0.2*intToDouble(t`mod` 240))

     drawCompleteTime = length lambdaRfoot

     moreVertices (a:b:cs) = if magnitude (a-b) > d then moreVertices (a:((a+b)/(2:+0)):b:cs) else a:moreVertices(b:cs)
       where d=6

     moreVertices x = x

     wing = [(30:+0),(200:+0),(216:+16),(208:+24),(224:+24),(240:+40),(232:+48),(248:+48),(272:+72),(168:+72)]

     renderVertices2D :: [Complex Double] -> IO ()
     renderVertices2D xys = mapM_ (\(x:+y) -> vertex $ Vertex3 x y 0) xys

     demoStartTime = if presentationMode then 480 else 1800

