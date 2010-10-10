{-# LANGUAGE ImplicitParams #-}
module RenderUtil
  ( renderWithShade
  , ugoVertexFreq
  , ugoVertices2D
  , ugoVertices2DFreq
  , ugoTranslate
  , ugoTranslateFreq
  , rotateX
  , rotateY
  , rotateZ 
  , modifyColor
  , translate1D
  , translate2D
  ) where

import GLWrapper

rotateX, rotateY, rotateZ :: Double -> IO ()
rotateX rad = rotate rad (Vector3 1 0 0)
rotateY rad = rotate rad (Vector3 0 1 0)
rotateZ rad = rotate rad (Vector3 0 0 1)

modifyColor f (Color3 r g b) = Color3 (f r) (f g) (f b)

translate1D x = translate $ Vector3 x 0 0
translate2D x y = translate $ Vector3 x y 0

renderWithShade :: Color3 Double -> Color3 Double -> IO () -> IO ()
renderWithShade colorA colorB rndrr = do
  color colorB
  preservingMatrix $ do
    translate $ Vector3 1 (-1) (-1)
    rndrr
  color colorA
  preservingMatrix rndrr

  -- renders a vertex at somewhere near (x y z),
  -- but the point wiggles around in ugoRange when each interval comes.
ugoVertexFreq x y z = ugoFreq f x y z
  where f (x',y',z') = vertex $ Vertex3 x' y' z'

ugoTranslate x y z ugoRange = ugoTranslateFreq x y z ugoRange standardUgoInterval
ugoTranslateFreq x y z = ugoFreq f x y z
  where f (x',y',z') = translate $ Vector3 x' y' z'


ugoVertices2D z r xys = ugoVertices2DFreq z r standardUgoInterval xys
ugoVertices2DFreq z r intrvl xys = mapM_ (\(x,y) -> ugoVertexFreq x y z r intrvl) xys

------------------------------------------------------------
-- private
standardUgoInterval :: Int
standardUgoInterval = 7

ugoFreq f x y z ugoRange interval = f (x+dr*cos theta, y+dr*sin theta, z)
  where
    flipper :: Double
    flipper = fromIntegral $ (?gameclock `div` interval) `mod` 1024
    dr = ugoRange * vibrator(phi)
    theta = (x + sqrt(2)*y + sqrt(3)*z + 573) * 400 * flipper
    phi   = (x + sqrt(3)*y + sqrt(7)*z + 106) * 150 * flipper
    vibrator a = 0.5 * (1 + sin a)
