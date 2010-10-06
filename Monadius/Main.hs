{- Copyright 2005 Hideyuki Tanaka & Takayuki Muranushi
  This program is distributed under the terms of the GNU General Public License.

   NOTE
 This project meant to list up, not to solve, many possible problems that will appear
 while writing a game in Haskell.
 Only nushio is responsible to the unreadability of these codes. -}
module Main (main) where

import Graphics.Rendering.OpenGL.GLU hiding (lookAt)
import qualified Graphics.Rendering.OpenGL.GLU as GLU (lookAt)
import Control.Exception -- (catch, ExitException(), throwIO)
import System.Exit -- (ExitSuccess())
import Prelude hiding (catch)

import Data.IORef
import Data.List (isSuffixOf, nub)
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

import Demo -- (ReplayInfo(), demoData)
import Recorder

import GLWrapper

import GlobalVariables
import qualified EndingProc as EP
import qualified OpeningProc as OP
import qualified MainProc as MP




lookAt :: Vertex3 Double -> Vertex3 Double -> Vector3 Double -> IO ()
lookAt v1 v2 v3 = GLU.lookAt (fmap r2f v1) (fmap r2f v2) (fmap r2f v3)


endingProc :: GlobalVariables -> IORef [Key] -> Double -> IO Scene
endingProc vars ks counter = do
   keystate <- readIORef ks
   let stage = (fst . saveState) vars
   s <- EP.endingProc stage keystate counter
   return $ Scene $ case s of
     (EP.Ending counter') -> endingProc vars ks counter'
     (EP.Opening)         -> openingProc vars ks 0 1

openingProc :: GlobalVariables -> IORef [Key] -> Int -> Int -> IO Scene
openingProc vars ks clock menuCursor = do
  keystate <- readIORef ks
  s <- OP.openingProc clock menuCursor vars keystate
  return $ Scene $ case s of
    (OP.Opening c' m' v') -> openingProc v' ks c' m'
    (OP.Main vars' gs)    -> mainProc vars' gs ks

mainProc :: GlobalVariables -> IORef Recorder -> IORef [Key] -> IO Scene
mainProc vars gs ks = do
  keystate <- readIORef ks
  s <- MP.mainProc vars gs keystate
  return $ Scene $ case s of
    (MP.Opening v') -> openingProc v' ks 0 1
    (MP.Ending v')  -> endingProc v' ks 0.0
    (MP.Main v' g') -> mainProc v' g' ks


loadReplay :: String-> IO ReplayInfo
loadReplay filename = readFile filename >>= (return . read)

main :: IO ()
main = do
  args <- getArgs
  _ <- getArgsAndInitialize

  (recMode,keys,rss,repName) <- if isJust $ getReplayFilename args then do
      ReplayInfo (ss,keystr) <- (loadReplay . fromJust . getReplayFilename) args
      return (Playback,decode keystr,ss,Just $ (simplify . fromJust . getReplayFilename) args)
    else if "-r" `elem` args then do
        return (Play,[],(1,0),Nothing)
      else
        return (Record,[],(1,0),Nothing)

  keystate <- newIORef []
  cp <- newIORef (openingProc GlobalVariables{saveState = (1,0) ,isCheat = False,
                                              recorderMode=recMode,playbackKeys=keys,playbackSaveState = rss,recordSaveState=(1,0),demoIndex=0,
                                              playBackName=repName,saveHiScore=0} keystate 0 0)
  initialWindowSize $= Size 640 480
  initialDisplayMode $= [RGBAMode,DoubleBuffered]

  wnd <- createWindow "Monadius"


  curwnd <- if "-f" `elem` args then do
    gameModeCapabilities $= [
        Where' GameModeWidth IsLessThan 650,
        Where' GameModeHeight IsLessThan 500
      ]

    (wnd2,_) <- enterGameMode
    destroyWindow wnd
    return wnd2
   else do
    return wnd

  displayCallback $= dispProc cp

  keyboardMouseCallback $= Just (keyProc keystate)
  addTimerCallback 16 (timerProc (dispProc cp))

  initMatrix

  mainLoop
  destroyWindow curwnd

  `catch` (\err -> print err)

  where
    getReplayFilename [] = Nothing
    getReplayFilename a = (Just . head . candidates) a

    candidates args = filter (replayFileExtension `isSuffixOf`) args

    simplify = (removesuffix . removedir)

    removedir str | '\\' `elem` str || '/' `elem` str = (removedir . tail) str
                  | otherwise = str
    removesuffix str | '.' `elem` str = (removesuffix . init) str 
                     | otherwise = str

exitLoop :: IO a
exitLoop = throwIO $ ExitException ExitSuccess

initMatrix :: IO ()
initMatrix = do
  viewport $= (Position 0 0,Size 640 480)
  matrixMode $= Projection
  loadIdentity
  perspective 30.0 (4/3) 600 1400
  lookAt (Vertex3 0 0 (927 :: Double)) (Vertex3 0 0 (0 :: Double)) (Vector3 0 1 (0 :: Double))

dispProc :: IORef (IO Scene) -> IO ()
dispProc cp = do
  m <- readIORef cp
  Scene next <- m
  writeIORef cp next

timerProc :: IO () -> IO ()
timerProc m = addTimerCallback 16 $ timerProc m >> m

keyProc :: IORef [Key] -> KeyboardMouseCallback
keyProc keystate key ks _ _ = do
  case (key,ks) of
    (Char 'q',_) -> exitLoop
    (Char '\ESC',_) -> exitLoop
    (_,Down) -> modifyIORef keystate (nub . (++ [key]))
    (_,Up) -> modifyIORef keystate (filter (/=key))

