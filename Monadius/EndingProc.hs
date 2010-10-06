module EndingProc
   ( endingProc
   , EndState(..)
   ) where

import Control.Monad (zipWithM_)

import GLWrapper

data EndState = Ending Double | Opening

cWhite :: IO ()
cWhite = color $ Color3 1.0 1.0 1.0

endingProc :: Int -> [Key] -> Double -> IO EndState
endingProc stage keystate counter = do
  initGraphics

  cWhite
  zipWithM_ (\str pos -> preservingMatrix $ do
    translate $ Vector3 (-180) (-240+counter-pos) 0
    scale 0.3 0.3 0.3
    renderString Roman str)
    (stuffRoll stage) [0,60..]

  swapBuffers

  return $ if Char ' ' `elem` keystate
     then Opening
     else Ending $ 2420 `min` counter + 2

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
