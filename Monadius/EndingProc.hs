module EndingProc
   ( endingProc
   , EndState(..)
   ) where

import Control.Monad (zipWithM_)

import Data.IORef

import GLWrapper

data EndState
   = Ending (IORef Double)
   | Opening Int Int

initGraphics :: IO ()
initGraphics = do
  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

endingProc :: Int -> [Key] -> IORef Double -> IO EndState
endingProc stage keystate ctr= do
  counter <- readIORef ctr
  modifyIORef ctr (min 2420 . (+2.0))

  initGraphics

  color $ Color3 (1.0 :: Double) 1.0 1.0
  zipWithM_ (\str pos -> preservingMatrix $ do
    translate $ Vector3 (-180 :: Double) (-240+counter-pos) 0
    scale (0.3 :: Double) 0.3 0.3
    renderString Roman str)
    (stuffRoll stage) [0,60..]

  swapBuffers

  if Char ' ' `elem` keystate
     then return $ Opening 0 1
     else return $ Ending ctr

stuffRoll :: Int -> [String]
stuffRoll stage = [
     "",
     "",
     "Game Designer",
     "    nushio",
     "",
     "Frame Programmer",
     "    tanakh",
     "",
     "Graphics Designer",
     "    Just nushio",
     "",
     "Sound Designer",
     "    Match Makers",
     "",
     "Lazy Evaluator",
     "    GHC 6.8",
     "",
     "Inspired"   ,
     "    Ugo-Tool",
     "    gradius2.com",
     "    Gradius series",
     "",
     "Special thanks to",
     "    John Peterson",
     "    Simon Marlow",
     "    Haskell B. Curry",
     "    U.Glasgow",
     "",
     "Presented by",
     "    team combat",
     "",
     "",
     if stage <= 2 then "Congratulations!" else "WE LOVE GAMES!!" ,
     "",
     "    press space key"]
