module GLWrapper
  ( module Graphics.UI.GLUT
  , vertex
  , translate
  , rotate
  , scale
  , color
  , r2f
  )where

import Graphics.UI.GLUT hiding (position, vertex, translate, rotate, scale, color, lookAt)
import qualified Graphics.UI.GLUT as G (vertex,translate, rotate, scale, color)

import Unsafe.Coerce

r2f :: Double -> GLdouble
r2f = unsafeCoerce -- fromRational . toRational

vertex v = G.vertex (fmap r2f v)
translate v = G.translate (fmap r2f v)
rotate r v = G.rotate (r2f r) (fmap r2f v)
scale a b c = G.scale (r2f a) (r2f b) (r2f c)
color v = G.color (fmap r2f v)

