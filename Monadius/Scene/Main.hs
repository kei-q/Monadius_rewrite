module Scene.Main
  ( scene
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
  = Ending GlobalVariables
  | Opening GlobalVariables 
  | Main GlobalVariables (IORef Recorder) 


when t action = if t then action else return ()

scene :: GlobalVariables -> IORef Recorder -> [Key] -> IO MainState
scene vars gs keystate = do
  modifyIORef gs (update keystate)
  gamestate <- readIORef gs

  initGraphics
  render gamestate
  swapBuffers

  let getV = getVariables $ gameBody gamestate
  let currentLevel = baseGameLevel getV
  let currentArea  = maximum $ filter (\i -> (savePoints !! i) < (gameClock getV)) [0..(length savePoints-1)]
  let currentSave  = if mode gamestate == Playback then saveState vars else (currentLevel,currentArea)
  let currentHi    = max (saveHiScore vars) (hiScore getV)

  let vars' = vars{saveState=currentSave,saveHiScore = currentHi}
  if (isGameover gamestate)
    then gameover vars vars' gamestate currentLevel
    else return $ Main vars' gs
     
gameover vars vars' gamestate currentLevel = do
  when (mode gamestate == Record) (writeReplay vars gamestate $ show (ReplayInfo (recordSaveState vars,(encode2 . preEncodedKeyBuf) gamestate)))

  return $ if currentLevel>1 && (not $ isCheat vars) && (mode gamestate /= Playback)
    then Ending vars'
    else Opening vars'

writeReplay vs gamestate str = do
  home <- getEnv "HOME"
  let replayFile =  home ++ replayFileName
  createDirectoryIfMissing True replayFile
  filename <- searchForNewFile (
      "replay\\" ++ (showsave . recordSaveState) vs ++ "-" ++ (showsave . saveState) vs ++ "." ++
      ((padding '0' 8) . show . totalScore . getVariables . gameBody) gamestate ++ "pts") 0
  writeFile (replayFile ++ filename) str
  where
    replayFileName = "/.monadius-replay/"

showsave (a,b) = show (a,b+1)

searchForNewFile prefix i = do
  let fn = prefix ++ (uniqStrs!!i) ++ replayFileExtension
  b <- doesFileExist fn
  if not b then return fn else do searchForNewFile prefix $ i + 1

uniqStrs = ("") : (map (("." ++) . show) ([1..] :: [Int]))
