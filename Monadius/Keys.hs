module Keys
  ( KeyF
  , toKeyF
  , keySpace
  ) where

import Graphics.UI.GLUT.Callbacks.Window (Key(..))

type KeyF = Key -> Bool

toKeyF ks k = k `elem` ks
keySpace = Char ' '

