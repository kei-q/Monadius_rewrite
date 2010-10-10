module Render.VicViper (render) where

import GLWrapper
import RenderUtil (renderWithShade, ugoVertices2D, ugoVertices2DFreq, translate2D)
import Util (intToDouble)

render (x,y) hp age | hp <= 0 = renderBang
                    | otherwise   = renderBody
    where
      renderBang = preservingMatrix $ do
        translate2D x y
        scale pishaMagnitudeX pishaMagnitudeY 0
        renderWithShade (Color3 1.0 0 0) (Color3 1.0 0.6 0.4) $ do
          renderPrimitive LineLoop $ ugoVertices2DFreq 0 1 1
            [(0,12),(8,8),(10,4),(20,0),(10,-4),(8,-8),(0,-12),(-8,-8),(-10,-4),(-20,0),(-10,4),(-8,8)]
        where
          pishaMagnitudeX = max 0 $ (8*) $ (\z -> z*(1-z)) $ (/20) $ intToDouble age
          pishaMagnitudeY = max 0 $ (5*) $ (\z -> z*(1-z)) $ (/15) $ intToDouble age
      renderBody = preservingMatrix $ do
        translate $ Vector3 x y 0
        renderWithShade (Color3 1.0 1.0 1.0) (Color3 0.4 0.4 0.6) $ do
          renderLine 0 2 [((-14),(-1)),((-12),5),((-20),13),(-14,13),(2,5),(8,1),(32,1),(32,(-1)),(24,(-3)),(16,(-3))]
          renderLine 0 2 [((-10),(-1)),(14,(-1)),(18,(-5)),(4,(-9)),((-2),(-9))]
          renderLine 0 2 [((-18),3),((-16),3),((-16),(-3)),((-18),(-3))]
        renderWithShade (Color3 0.92 0.79 0.62) (Color3 0.75 0.38 0.19) $ do
          renderLine 0 2 vCockpit
        renderWithShade (Color3 0.6 0.8 1.0) (Color3 0.19 0.38 0.75) $ do
          renderLine 0 2 vCoting
        renderWithShade (Color3 0 0 0.8) (Color3 0.0 0.0 0.4) $ do
          renderLine 0 4 vBackfire
        where
          renderLine x y vs = renderPrimitive LineLoop $ ugoVertices2D x y vs
          vCockpit = [(4,3),(6,5),(14,5),(22,1)] --cockpit
          vCoting = [((-14),(-1)),((-10),(-1)),((-2),(-9)),((-4),(-9)),((-10),(-7)),((-14),(-3))] -- identification blue coting
          vBackfire = [((-36),1),((-28),5),((-24),5),((-20),1),((-20),(-1)),((-24),(-5)),((-28),(-5)),((-36),(-1))] -- backfire

