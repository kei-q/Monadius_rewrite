{-# LANGUAGE RankNTypes #-}
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

import System.FilePath (takeBaseName)
import Data.IORef
import Data.List (isSuffixOf, nub)
import System.Environment (getArgs)

import Demo -- (ReplayInfo(), demoData)
import Recorder

import GLWrapper

import GlobalVariables
import qualified Scene.Ending as SE
import qualified Scene.Opening as SO
import qualified Scene.Main as SM

import Keys

lookAt :: Vertex3 Double -> Vertex3 Double -> Vector3 Double -> IO ()
lookAt v1 v2 v3 = GLU.lookAt (fmap r2f v1) (fmap r2f v2) (fmap r2f v3)


sceneProc :: forall a a1. IORef a -> (a -> IO a1) -> (a1 -> IO Scene) -> IO Scene
sceneProc ks proc next = readIORef ks >>= proc >>= return . Scene . next

endingProc :: IORef [Key] -> GlobalVariables -> Double -> IO Scene
endingProc ks vars counter = do -- sceneProc ks proc next
  ks' <- readIORef ks
  scene <- proc (toKeyF ks')
  return $ Scene $ next scene
  where
    proc = SE.scene (fst $ saveState vars) counter
    next (SE.Next c') = endingProc ks vars c'
    next SE.End       = openingProc ks vars (0,1)

openingProc :: IORef [Key] -> GlobalVariables -> (Int,Int) -> IO Scene
openingProc ks vars s = sceneProc ks proc next
  where
    proc = SO.scene s vars
    next (SO.Opening s' v') = openingProc ks v' s'
    next (SO.Main v' gs) = mainProc ks v' gs

mainProc :: IORef [Key] -> GlobalVariables -> IORef Recorder -> IO Scene
mainProc ks vars gs = sceneProc ks proc next
  where
    proc = SM.scene vars gs
    next (SM.Opening v') = openingProc ks v' (0,1)
    next (SM.Ending v')  = endingProc ks v' 0.0
    next (SM.Main v' g') = mainProc ks v' g'


readRecordSettings args
  | Just replay <- getReplayFilename args = do
    ReplayInfo (ss,keystr) <- loadReplay replay
    return (Playback,decode keystr,ss,Just $ takeBaseName replay)
  | otherwise = do
    let mode = if "-r" `elem` args then Play else Record
    return (mode, [], (1,0), Nothing)
  where
    getReplayFilename [] = Nothing
    getReplayFilename a = (Just . head . candidates) a
    loadReplay :: String-> IO ReplayInfo
    loadReplay filename = readFile filename >>= (return . read)
    candidates args = filter (replayFileExtension `isSuffixOf`) args

main :: IO ()
main = do
  args <- getArgs
  _ <- getArgsAndInitialize

  (recMode,keys,rss,repName) <- readRecordSettings args

  keystate <- newIORef []
  cp <- newIORef (openingProc keystate GlobalVariables{saveState = (1,0) ,isCheat = False,
                                              recorderMode=recMode,playbackKeys=keys,playbackSaveState = rss,recordSaveState=(1,0),demoIndex=0,
                                              playBackName=repName,saveHiScore=0} (0,0))
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

-- error on base 4.x
--  `catch` (\_ -> return ())

exitLoop :: IO a
exitLoop = throwIO ExitSuccess

initMatrix :: IO ()
initMatrix = do
  viewport $= (Position 0 0,Size 640 480)
  matrixMode $= Projection
  loadIdentity
  perspective 30.0 (4/3) 600 1400
  lookAt (Vertex3 0 0 927) (Vertex3 0 0 0) (Vector3 0 1 0)

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

