module Scene.Ending
   ( scene
   , EndScene(..)
   ) where

import Control.Monad (zipWithM_)

import GLWrapper
import Keys

data EndScene = Next Double | End


cWhite :: IO ()
cWhite = color $ Color3 1.0 1.0 1.0

scene :: Int -> Double -> KeyF -> IO EndScene
scene _ _ keys | keys keySpace = return End
scene stage counter _ = do
  initGraphics

  cWhite
  zipWithM_ (\str pos -> preservingMatrix $ do
    translate $ Vector3 (-180) (-240+counter-pos) 0
    scale 0.3 0.3 0.3
    renderString Roman str)
    (stuffRoll stage) [0,60..]

  swapBuffers
  return $ Next (2420 `min` counter + 2)

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
