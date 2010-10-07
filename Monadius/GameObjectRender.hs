module GameObjectRender
  ( renderMonadius
  ) where

import Data.Complex
import Util
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Array ((!))
import Data.List (find)

import GLWrapper
import GameObject
import GOConst

standardUgoInterval :: Int
standardUgoInterval = 7

-------------------------
--
--  drawing
--
-------------------------
renderMonadius :: Monadius -> IO ()
renderMonadius (Monadius (variables,objects)) = do
  putDebugStrLn $ show $ length objects
  mapM_ renderGameObject objects
  preservingMatrix $ do
    translate (Vector3 (-300) (220) (0 :: Double))
    renderWithShade (Color3 1 1 (1 :: Double)) (Color3 0 0 (1 :: Double)) $ do
      scale (0.2 :: Double) 0.2 0.2
      renderString MonoRoman scoreStr
  preservingMatrix $ do
    translate (Vector3 (0) (220) (0 :: Double))
    renderWithShade (Color3 1 1 (1 :: Double)) (Color3 0 0 (1 :: Double)) $ do
      scale (0.2 :: Double) 0.2 0.2
      renderString MonoRoman scoreStr2
  where
  scoreStr = "1P "  ++ ((padding '0' 8).show.totalScore) variables
  scoreStr2 = if isNothing $ playTitle variables then "HI "++((padding '0' 8).show.hiScore) variables else (fromJust $ playTitle variables)

  gameclock = gameClock variables

  -- returns an IO monad that can render the object.
  renderGameObject :: GameObject -> IO ()
  renderGameObject gauge@PowerUpGauge{} = preservingMatrix $ do
    let x:+y = position gauge
    translate (Vector3 x y 0)
    color (Color3 (1.0 :: Double) 1.0 1.0)
    mapM_ (\(i,j) -> (if(i==activeGauge)then renderActive else renderNormal) j (isLimit i) i) $
      zip [0..5] [0,90..450] where
      w=80
      h=20
      renderNormal x l i = preservingMatrix $ do
        color (Color3 0.7 0.8 (0.8 :: Double))
        preservingMatrix $ do
          translate (Vector3 x 0 (0 :: Double))
          renderPrimitive LineLoop $ ugoVertices2D 0 1 [(0,0),(w,0),(w,h),(0,h)]
          if l then renderPrimitive Lines $ ugoVertices2D 0 1 [(0,0),(w,h),(w,0),(0,h)] else return()
        preservingMatrix $ do
          ugoTranslate x 0 0 3
          translate (Vector3 (w/2) 0 (0 :: Double))
          rotate (3 * sin(intToDouble gameclock/10)) (Vector3 0 0 (1 :: Double))
          translate (Vector3 (-w/2) 0 (0 :: Double))
          renderPowerUpName i

      renderActive x l i = preservingMatrix $ do
        color (Color3 1 1 (0 :: Double))
        preservingMatrix $ do
          translate (Vector3 x 0 0)
          renderPrimitive LineLoop $ ugoVertices2DFreq 0 5 2 [(0,0),(w,0),(w,h),(0,h)]
          if l then renderPrimitive Lines $ ugoVertices2DFreq 0 5 2 [(0,0),(w,h),(w,0),(0,h)] else return()
        preservingMatrix $ do
          ugoTranslateFreq x 0 0 5 2
          translate (Vector3 (w/2) 0 (0 :: Double))
          rotate (10 * sin(intToDouble gameclock/5)) (Vector3 0 0 (1 :: Double))
          scale 1.2 1.2 (0 :: Double)
          translate (Vector3 (-w/2) 0 (0 :: Double))
          renderPowerUpName i
      activeGauge = powerUpPointer vicViper
      isLimit i = powerUpLevels vicViper!i>=powerUpLimits!!i
      renderPowerUpName i = do
        translate (Vector3 6 3.5 (0 :: Double))
        scale (0.15 :: Double) 0.13 0.15
        renderString Roman $ ["SPEED","MISSILE","DOUBLE","LASER","OPTION","  ?"]!!i

  renderGameObject vic@VicViper{position = x:+y} = if hp vic<=0 then preservingMatrix $ do
      translate (Vector3 x y 0)
      scale pishaMagnitudeX pishaMagnitudeY 0
      renderWithShade (Color3 (1.0 :: Double) 0 0) (Color3 (1.0 :: Double) 0.6 0.4) $ do
        renderPrimitive LineLoop $ ugoVertices2DFreq 0 1 1
          [(0,12),(8,8),(10,4),(20,0),(10,-4),(8,-8),(0,-12),(-8,-8),(-10,-4),(-20,0),(-10,4),(-8,8)]
    else preservingMatrix $ do
      translate (Vector3 x y 0)
      renderWithShade (Color3 (1.0 :: Double) 1.0 1.0) (Color3 (0.4 :: Double) 0.4 0.6) $ do
        renderPrimitive LineStrip $ ugoVertices2D 0 2
          [((-14),(-1)),((-12),5),((-20),13),(-14,13),(2,5),(8,1),(32,1),(32,(-1)),(24,(-3)),(16,(-3))]
        renderPrimitive LineStrip $ ugoVertices2D 0 2
          [((-10),(-1)),(14,(-1)),(18,(-5)),(4,(-9)),((-2),(-9))]
        renderPrimitive LineLoop $ ugoVertices2D 0 2
          [((-18),3),((-16),3),((-16),(-3)),((-18),(-3))]
      renderWithShade (Color3 (0.92 :: Double) 0.79 0.62) (Color3 (0.75 :: Double) 0.38 0.19) $ do
        renderPrimitive LineStrip $ ugoVertices2D 0 2
          [(4,3),(6,5),(14,5),(22,1)] --cockpit
      renderWithShade (Color3 (0.6 :: Double) 0.8 1.0) (Color3 0.19 0.38 (0.75 :: Double)) $ do
        renderPrimitive LineLoop $ ugoVertices2D 0 2
          [((-14),(-1)),((-10),(-1)),((-2),(-9)),((-4),(-9)),((-10),(-7)),((-14),(-3))] -- identification blue coting
      renderWithShade (Color3 (0 :: Double) 0 0.8) (Color3 (0.0 :: Double) 0.0 0.4) $ do
        renderPrimitive LineLoop $ ugoVertices2D 0 4
          [((-36),1),((-28),5),((-24),5),((-20),1),((-20),(-1)),((-24),(-5)),((-28),(-5)),((-36),(-1))] -- backfire
    where
      pishaMagnitudeX :: Double
      pishaMagnitudeY :: Double
      pishaMagnitudeX = max 0 $ (8*) $ (\z -> z*(1-z)) $ (/20) $ intToDouble $ ageAfterDeath vic
      pishaMagnitudeY = max 0 $ (5*) $ (\z -> z*(1-z)) $ (/15) $ intToDouble $ ageAfterDeath vic

  renderGameObject Option{position = x:+y} = preservingMatrix $ do
    translate (Vector3 x y 0)
    renderWithShade (Color3 (0.8 :: Double) 0 0) (Color3 (0.4 :: Double) 0 0) $
      renderPrimitive LineLoop $ ugoVertices2D 0 2
        [(5,9),(9,7),(13,3),(13,(-3)),(9,(-7)),(5,(-9)),
         ((-5),(-9)),((-9),(-7)),((-13),(-3)),((-13),3),((-9),7),((-5),9)]
    renderWithShade (Color3 (1.0 :: Double) 0.45 0) (Color3 (0.4 :: Double) 0.2 0) $
      renderPrimitive LineStrip $ ugoVertices2D 0 1
        [((-12.0),(3.4)),(0.8,8.7),((-8.1),(-0.9)),(4.0,5.8),(4.3,5.6),
          ((-4.4),(-6.8)),((-4.1),(-6.9)),(8.3,0.8),(9.0,0.6),(2.0,(-7.2))]

  renderGameObject StandardMissile{position=x:+y,velocity=v} = preservingMatrix $ do
    let dir = (phase v) :: Double
    translate (Vector3 x y 0)
    rotate (dir / pi * 180) (Vector3 0 0 (1 :: Double))
    color (Color3 (1.0 :: Double) 0.9 0.5)
    renderPrimitive LineLoop $ ugoVertices2D 0 1 [(0,0),(-7,2),(-7,-2)]
    renderPrimitive LineStrip $ ugoVertexFreq (-11) 0 0 1 1 >> ugoVertexFreq (-17) 0 0 7 1

  renderGameObject StandardRailgun{position=x:+y,velocity=v} =
    preservingMatrix $ do
      let (_,phse)=polar v
      translate (Vector3 x y 0)
      rotate (phse / pi * 180) (Vector3 0 0 (1 :: Double))
      color (Color3 (1.0 :: Double) 0.9 0.5)
      renderPrimitive Lines $ ugoVertices2D 0 1 [(0,0),((-5),0),((-9),0),((-11),0)]

  renderGameObject laser@StandardLaser{position=x:+y,velocity=v} =
    if age laser < 1 then return ()
    else preservingMatrix $ do
      let (_,phs)=polar v
      translate (Vector3 x y 0)
      rotate (phs / pi * 180) (Vector3 0 0 (1 :: Double))
      color (Color3 (0.7 :: Double) 0.9 1.0)
      renderPrimitive Lines $ ugoVertices2D 0 0 [(12,0),(-laserSpeed,0)]

  renderGameObject Shield{position=x:+y, size = r,angle = theta} = preservingMatrix $ do
    translate (Vector3 x y 0)
    rotate theta (Vector3 0 0 (1 :: Double))
    renderWithShade (Color3 (0.375 :: Double) 0.75 0.9375) (Color3 (0.86 :: Double) 0.86 0.86) $ do
      scale r r 0
      renderTriangle
      rotate 60 (Vector3 0 0 (1 :: Double))
      renderTriangle where
        renderTriangle = do
          renderPrimitive LineLoop $ ugoVertices2DFreq 0 0.1 1 $ map (\t -> (cos t,sin t)) [0,pi*2/3,pi*4/3]

  renderGameObject powerUpCapsule@PowerUpCapsule{} = preservingMatrix $ do
    let x:+y = position powerUpCapsule
    translate (Vector3 x y 0)
    renderWithShade (Color3 (0.9 :: Double) 0.9 0.9) (Color3 (0.4 :: Double) 0.4 0.4) $ do
      futa >> neji >> toge
      rotate (180) (Vector3 1 0 (0 :: Double)) >> toge
      rotate (180) (Vector3 0 1 (0 :: Double)) >> futa >> neji >> toge
      rotate (180) (Vector3 1 0 (0 :: Double)) >> toge
    renderWithShade (Color3 (1.0 :: Double) 0.0 0.0) (Color3 (0.3 :: Double) 0.3 0.0) $ do
      nakami
      where
        futa = renderPrimitive LineStrip $ ugoVertices2D 0 1 [((-10),6),((-6),10),(6,10),(10,6)]
        neji = (renderPrimitive LineStrip $ ugoVertices2D 0 1 [(12,4),(12,(-4))]) >>
               (renderPrimitive LineStrip $ ugoVertices2D 0 1 [(16,2),(16,(-2))])
        toge = renderPrimitive LineStrip $ ugoVertices2D 0 1 [(10,8),(16,14)]
        nakami = rotate 145 (Vector3 0.2 0.2 (1 :: Double)) >> scale 9 6 (1 :: Double) >>
          (renderPrimitive LineStrip $ ugoVertices2D 0 0.2 $ map (\n ->  (cos$n*pi/8,sin$n*pi/8)) [1,15,3,13,5,11,7,9])

  renderGameObject DiamondBomb{position = (x:+y),age=clock} = preservingMatrix $ do
    translate (Vector3 x y 0)
    rotate (90*intToDouble(clock`mod`4)) (Vector3 0 0 (1 :: Double))
    color (Color3 (1 :: Double) 1 1)
    renderPrimitive LineLoop $ vertices2D 0 $ [a,b,c]
    color (Color3 (0.5 :: Double) 0.5 0.5)
    renderPrimitive Lines $ vertices2D 0 $ [a,d,a,e]
    renderPrimitive LineStrip $ vertices2D 0 $ [c,d,e,b]
    where
      [a,b,c,d,e] = [(0,0),(r,0),(0,r),(-r,0),(0,-r)]
      r = diamondBombSize
      --    c
      --   /|\
      --  d-a-b
      --   \|/
      --    e
  renderGameObject TurnGear{position=x:+y,age=clock} = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (Color3 1.0 0.7 1.0 :: Color3 Double)
    rotate (5 * intToDouble clock) (Vector3 0 0 1 :: Vector3 Double)
    renderWing
    rotate 120 (Vector3 0 0 1 :: Vector3 Double)
    renderWing
    rotate 120 (Vector3 0 0 1 :: Vector3 Double)
    renderWing
    where
      renderWing = renderPrimitive LineLoop $ ugoVertices2D 0 2 $ map ((\(t:+u) -> (t,u)) . (\(r,t) -> mkPolar r (pi*t)) )
        [(3,0), (3,2/3), (smallBacterianSize,1/3), (smallBacterianSize,0), (smallBacterianSize+3,-1/3)]

  renderGameObject Flyer{position=x:+y,age=_,velocity = v,hasItem=item}  = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 Double) else (Color3 0.3 1.0 0.7 :: Color3 Double))
    rotate (phase v / pi * 180) (Vector3 0 0 (1 :: Double))
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ [(-2,0),(-6,4),(-10,0),(-6,-4)]
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ [(2,4),(16,4),(4,16),(-10,16)]
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ [(2,-4),(16,-4),(4,-16),(-10,-16)]

  renderGameObject Ducker{position = (x:+y),hitDisp=hd,hasItem=item,velocity = v,gVelocity = g,age = a} = preservingMatrix $ do
    translate (Vector3 x y 0)
    if signum (imagPart g) > 0 then scale 1 (-1) (1 :: Double) else return ()
    if signum (realPart v) < 0 then scale (-1) 1 (1 :: Double) else return ()
    --after this, ducker is on the lower ground, looking right
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 Double) else (Color3 0.3 1.0 0.7 :: Color3 Double))
    renderShape (0:+0) hd
    renderPrimitive LineStrip $ vertices2D 0 [(0,0),(kx,ky),(fx,fy)]
    where
      fx:+fy=foot $ intToDouble a/2
      kx:+ky=knee $ intToDouble a/2
      foot theta = (16*cos(-theta)):+(-16+8*sin(-theta))
      knee theta = foot theta * (0.5 :+ (- sqrt(square(legLen/magnitude(foot theta)) - 0.25)))
      legLen = 16

  renderGameObject Jumper{position = (x:+y),hitDisp=hd,hasItem=item,gravity = g,velocity=v} = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 Double) else (Color3 0.3 1.0 0.7 :: Color3 Double))
    renderShape (0:+0) hd
    if gsign >0 then rotate 180 (Vector3 (1 :: Double) 0 0) else return() -- after this you can assume that the object is not upside down
    renderPrimitive LineStrip $ ugoVertices2D 0 2 $ [(15,-5),(25,-5+absvy*leg),(25,-25+absvy*leg)]
    renderPrimitive LineStrip $ ugoVertices2D 0 2 $ [(-15,-5),(-25,-5+absvy*leg),(-25,-25+absvy*leg)]
    where
      gsign = signum $ imagPart g
      absvy = imagPart v * gsign -- if falling (+) ascending (-)
      leg = 1.5

  renderGameObject Grashia{position = (x:+y),hitDisp=hd,hasItem=item,gunVector = nv,gravity = g,mode=m} = preservingMatrix $ do
    color (if item then (Color3 1.0 0.2 0.2 :: Color3 Double) else (Color3 0.3 1.0 0.7 :: Color3 Double))
    translate (Vector3 x y 0)
    renderShape (0:+0) hd
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ map (\r -> (nvx*r,nvy*r)) [16,32]
    if m == 1 then do
      renderShape 0 $ Circular (16:+12*gsign) 4
      renderShape 0 $ Circular ((-16):+12*gsign) 4
     else return ()
    where
      nvx:+nvy = nv
      gsign = signum $ imagPart g

  renderGameObject me@ScrambleHatch{position = (x:+y),hitDisp=_,gravity= g,gateAngle = angl} = preservingMatrix $ do
    translate (Vector3 x y 0)
    color (Color3 (1.2*(1-hpRate)) 0.5 (1.6*hpRate)  :: Color3 Double)
    if gsign >0 then rotate 180 (Vector3 (1 :: Double) 0 0) else return() -- after this you can assume that the object is not upside down
    renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*2) 1 $ [(-45,1),(-45,hatchHeight),(45,hatchHeight),(45,1)]
    preservingMatrix $ do
      translate (Vector3 45 hatchHeight (0 :: Double))
      rotate (-angl/pi*180) (Vector3 0 0 (1 :: Double))
      renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*1) 2 $ [(0,0),(-45,0),(-45,10)]
    preservingMatrix $ do
      translate (Vector3 (-45) hatchHeight (0 :: Double))
      rotate (angl/pi*180) (Vector3 0 0 (1 :: Double))
      renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*1) 2 $ [(0,0),(45,0),(45,10)]
    where
      gsign = signum $ imagPart g
      hpRate = (intToDouble $ hp me)/(intToDouble hatchHP)

  renderGameObject LandScapeBlock{position=pos,hitDisp=hd} = preservingMatrix $ do
    color (Color3 0.6 0.2 0 :: Color3 Double)
    renderShape pos hd
    if treasure!!(baseGameLevel variables) then do
      color (Color3 0.7 0.23 0 :: Color3 Double)
      translate (Vector3 0 0 (60 :: Double))
      renderShape pos hd
      color (Color3 0.5 0.17 0 :: Color3 Double)
      translate (Vector3 0 0 (-120 :: Double))
      renderShape pos hd
     else return()

  renderGameObject me@Particle{position = x:+y,particleColor=Color3 mr mg mb} = preservingMatrix $ do
    if age me>=0 then do
      translate (Vector3 x y 0)
      color (Color3 r g b)
      renderShape (0:+0) $ Circular (0:+0) (size me*extent)
      else return ()
    where
      extent = 0.5 +  intCut (intToDouble(age me) / decayTime me)
      decay =  exp $  intCut $ -intToDouble (age me) / decayTime me
      whiteout = exp $ intCut $  -2*intToDouble (age me) / decayTime me
      r = mr * decay + whiteout
      g = mg * decay + whiteout
      b = mb * decay + whiteout
      intCut :: Double -> Double
      intCut = intToDouble.round

  renderGameObject Star{position = x:+y,particleColor=c} = preservingMatrix $ do
    color c
    renderPrimitive LineStrip $ ugoVertices2D 0 2 [(0.1+x,0+y),(-0.1+x,0+y)]


  renderGameObject DebugMessage{debugMessage=str} =
    putDebugStrLn str

  renderGameObject _ = return ()
  vicViper = fromJust $ find (\obj -> case obj of
                            VicViper{} -> True
                            _          -> False) objects

  renderShape :: Complex Double -> Shape -> IO ()
  renderShape (x:+y) s = case s of
    Rectangular{bottomLeft = (l:+b), topRight = (r:+t)}  ->
      renderPrimitive LineLoop $ vertices2D 0 [(x+l,y+b),(x+l,y+t),(x+r,y+t),(x+r,y+b)]
    Circular{center=cx:+cy, radius = r} -> preservingMatrix $ do
      translate (Vector3 (cx+x) (cy+y) 0)
      rotate (intToDouble gameclock*(45+pi)) (Vector3 0 0 (1 :: Double))
      scale r r 1
      renderPrimitive LineLoop $ vertices2D 0 $ map (\t -> (cos(2/7*t*pi),sin(2/7*t*pi))) [0..6]
    Shapes{children=cs} -> mapM_ (renderShape (x:+y)) cs


  renderWithShade :: Color3 Double -> Color3 Double -> IO () -> IO ()
  renderWithShade colorA colorB rndrr = do
    color colorB
    preservingMatrix $ do
      translate $ Vector3 1 (-1) (-1 :: Double)
      rndrr
    color colorA
    preservingMatrix rndrr

--   ugoVertex :: Double -> Double -> Double -> Double -> IO ()
--   ugoVertex x y z r = ugoVertexFreq x y z r standardUgoInterval

  ugoVertexFreq :: Double -> Double -> Double -> Double -> Int -> IO ()
  -- renders a vertex at somewhere near (x y z),
  -- but the point wiggles around in ugoRange when each interval comes.
  ugoVertexFreq x y z ugoRange intrvl = vertex $ Vertex3 (x+dr*cos theta) (y+dr*sin theta) z where
    flipper :: Double
    flipper = fromIntegral $ (gameclock `div` intrvl) `mod` 1024
    dr = ugoRange * vibrator(phi)
    theta = (x + sqrt(2)*y + sqrt(3)*z + 573) * 400 * flipper
    phi   = (x + sqrt(3)*y + sqrt(7)*z + 106) * 150 * flipper
    vibrator a = 0.5 * (1 + sin a)

  ugoTranslate x y z ugoRange = ugoTranslateFreq x y z ugoRange standardUgoInterval
  ugoTranslateFreq x y z ugoRange intvl = translate (Vector3 (x+dr*cos theta) (y+dr*sin theta) z) where
    flipper :: Double
    flipper = fromIntegral $ (gameclock `div` intvl) `mod` 1024
    dr = ugoRange * vibrator(phi)
    theta = (x + sqrt(2)*y + sqrt(3)*z + 573) * 400 * flipper
    phi   = (x + sqrt(3)*y + sqrt(7)*z + 106) * 150 * flipper
    vibrator a = 0.5 * (1 + sin a)

  ugoVertices2D z r xys = ugoVertices2DFreq z r standardUgoInterval xys
  ugoVertices2DFreq z r intrvl xys = mapM_ (\(x,y) -> ugoVertexFreq x y z r intrvl) xys

  vertices2D :: Double -> [(Double,Double)] -> IO ()
  vertices2D z xys = mapM_ (\(x,y) -> vertex $ Vertex3 x y z) xys

