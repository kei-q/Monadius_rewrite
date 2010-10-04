module GlobalVariables where

import Recorder (RecorderMode)
import GLWrapper (Key)

data GlobalVariables = GlobalVariables{
  saveState :: (Int,Int) ,isCheat :: Bool, demoIndex :: Int,
  -- | 'recorderMode' means general gamemode that user wants,
  -- 'mode' of a recorder means current gamemode.
  -- two are different when temporal demo replays.
  recorderMode :: RecorderMode,
  playbackKeys :: [[Key]],playbackSaveState :: (Int,Int),playBackName :: Maybe String,
  recordSaveState :: (Int,Int),saveHiScore :: Int
  }

-- | Scene is something that does some IO,
-- then returns the Scene that are to be executed in next frame.
newtype Scene = Scene (IO Scene)

savePoints :: [Int]
savePoints = [0,1280,3000,6080]

replayFileExtension :: String
replayFileExtension = ".replay"

