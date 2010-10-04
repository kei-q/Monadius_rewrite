module MainProc
  ( mainProc
  , MainState(..)
  ) where

import Data.IORef
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)

import Demo -- (ReplayInfo(), demoData)
import Game (isGameover, render, update)
import Monadius
import Recorder
import Util (padding)

import GLWrapper

import GlobalVariables

data MainState
  = Ending GlobalVariables (IORef Double)
  | Opening Int Int GlobalVariables 
  | Main GlobalVariables (IORef Recorder) 

mainProc :: GlobalVariables -> IORef Recorder -> [Key] -> IO MainState
mainProc vars gs keystate = do
  modifyIORef gs (update keystate)
  gamestate <- readIORef gs

  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  render gamestate
  swapBuffers
  let currentLevel = baseGameLevel$getVariables$gameBody gamestate
  let currentArea = maximum $ filter (\i -> (savePoints !! i) < (gameClock $ getVariables $ gameBody gamestate)) [0..(length savePoints-1)]
  let currentSave = if mode gamestate == Playback then saveState vars else (currentLevel,currentArea)
  let currentHi = max (saveHiScore vars) (hiScore$getVariables$gameBody gamestate)
  if (isGameover gamestate) then do
      counter <- newIORef (0.0 :: Double)
      if mode gamestate /= Record then return () else do
        writeReplay vars gamestate $ show (ReplayInfo (recordSaveState vars,(encode2 . preEncodedKeyBuf) gamestate))

      if currentLevel>1 && (not . isCheat) vars && (mode gamestate /= Playback) then
        return $ Ending vars{saveState=currentSave,saveHiScore = currentHi} counter
       else return $ Opening 0 1 vars{saveState=currentSave,saveHiScore = currentHi}
    else return $ Main vars{saveState=currentSave,saveHiScore = currentHi} gs
  where
    writeReplay vs gamestate str = do
      home <- getEnv "HOME"
      createDirectoryIfMissing True (home ++ "/.monadius-replay/")
      filename <- searchForNewFile (
          "replay\\" ++ (showsave . recordSaveState) vs ++ "-" ++ (showsave . saveState) vs ++ "." ++
          ((padding '0' 8) . show . totalScore . getVariables . gameBody) gamestate ++ "pts") 0
      writeFile (home ++ "/.monadius-replay/" ++ filename) str
    showsave (a,b) = show (a,b+1)
    searchForNewFile prefix i = do
      let fn = prefix ++ (uniqStrs!!i) ++ replayFileExtension
      b <- doesFileExist fn
      if not b then return fn else do
        searchForNewFile prefix $ i + 1
    uniqStrs = ("") : (map (("." ++) . show) ([1..] :: [Int]))
