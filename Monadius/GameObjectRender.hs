module GameObjectRender
  ( renderMonadius
  ) where

import Data.Complex
import Util
import Data.Array ((!))

import GLWrapper
import GameObject
import GOConst

when b act = if b then act else return ()

rotateX, rotateY, rotateZ :: Double -> IO ()
rotateX rad = rotate rad (Vector3 1 0 0)
rotateY rad = rotate rad (Vector3 0 1 0)
rotateZ rad = rotate rad (Vector3 0 0 1)

modifyColor f (Color3 r g b) = Color3 (f r) (f g) (f b)

translate1D x = translate $ Vector3 x 0 0
translate2D x y = translate $ Vector3 x y 0


standardUgoInterval :: Int
standardUgoInterval = 7

renderWithShade :: Color3 Double -> Color3 Double -> IO () -> IO ()
renderWithShade colorA colorB rndrr = do
  color colorB
  preservingMatrix $ do
    translate $ Vector3 1 (-1) (-1)
    rndrr
  color colorA
  preservingMatrix rndrr

renderScore :: Vector3 Double -> String -> IO ()
renderScore pos str = preservingMatrix $ do
  translate pos
  renderWithShade (Color3 1 1 1) (Color3 0 0 1) $ do
    scale 0.2 0.2 0.2
    renderString MonoRoman str

-------------------------
--
--  drawing
--
-------------------------
renderMonadius :: Monadius -> IO ()
renderMonadius (Monadius (variables,objects)) = do
  mapM_ renderGameObject objects
  renderScore (Vector3 (-300) 220 0) scoreStr
  renderScore (Vector3 (   0) 220 0) scoreStr2
  where
  scoreStr = "1P "  ++ ((padding '0' 8).show.totalScore) variables
  scoreStr2 | Just score <- playTitle variables = score
            | otherwise = "HI "++((padding '0' 8).show.hiScore) variables

  gameclock = gameClock variables

  -- returns an IO monad that can render the object.
  renderGameObject :: GameObject -> IO ()
  renderGameObject gauge@PowerUpGauge{} = preservingMatrix $ do
    let x:+y = position gauge
    translate2D x y
    color $ Color3 1.0 1.0 1.0
    mapM_ (\(i,j) -> (if(i==activeGauge)then renderActive else renderNormal) j (isLimit i) i) $
      zip [0..5] [0,90..] where
      w=80
      h=20
      box = [(0,0),(w,0),(w,h),(0,h)]
      cross = [(0,0),(w,h),(w,0),(0,h)]
      
      renderFrame ugoF x l = preservingMatrix $ do
        translate1D x
        renderPrimitive LineLoop $ ugoF box
        when l $ renderPrimitive Lines $ ugoF cross

      renderNormal x l i = preservingMatrix $ do
        color $ Color3 0.7 0.8 0.8
        renderFrame (ugoVertices2D 0 1) x l
        preservingMatrix $ do
          ugoTranslate x 0 0 3
          translate1D (w/2)
          rotateZ (3 * sin(intToDouble gameclock/10))
          translate1D (-w/2)
          renderPowerUpName i

      renderActive x l i = preservingMatrix $ do
        color $ Color3 1 1 0
        renderFrame (ugoVertices2DFreq 0 5 2) x l 
        preservingMatrix $ do
          ugoTranslateFreq x 0 0 5 2
          translate1D (w/2)
          rotateZ (10 * sin(intToDouble gameclock/5))
          scale 1.2 1.2 0
          translate1D (-w/2)
          renderPowerUpName i
      activeGauge = powerUpPointer vicViper
      isLimit i = powerUpLevels vicViper!i>=powerUpLimits!!i
      renderPowerUpName i = do
        translate2D 6 3.5
        scale 0.15 0.13 0.15
        renderString Roman $ ["SPEED","MISSILE","DOUBLE","LASER","OPTION","  ?"]!!i

  renderGameObject vic@VicViper{position = x:+y} 
    | hp vic<=0 = renderBang 
    | otherwise = renderBody
    where
      renderBang = preservingMatrix $ do
        translate2D x y
        scale pishaMagnitudeX pishaMagnitudeY 0
        renderWithShade (Color3 1.0 0 0) (Color3 1.0 0.6 0.4) $ do
          renderPrimitive LineLoop $ ugoVertices2DFreq 0 1 1
            [(0,12),(8,8),(10,4),(20,0),(10,-4),(8,-8),(0,-12),(-8,-8),(-10,-4),(-20,0),(-10,4),(-8,8)]
        where
          pishaMagnitudeX :: Double
          pishaMagnitudeY :: Double
          pishaMagnitudeX = max 0 $ (8*) $ (\z -> z*(1-z)) $ (/20) $ intToDouble $ ageAfterDeath vic
          pishaMagnitudeY = max 0 $ (5*) $ (\z -> z*(1-z)) $ (/15) $ intToDouble $ ageAfterDeath vic
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

  renderGameObject Option{position = x:+y} = preservingMatrix $ do
    translate2D x y
    renderWithShade (Color3 0.8 0 0) (Color3 0.4 0 0) $
      renderPrimitive LineLoop $ ugoVertices2D 0 2
        [(5,9),(9,7),(13,3),(13,(-3)),(9,(-7)),(5,(-9)),
         ((-5),(-9)),((-9),(-7)),((-13),(-3)),((-13),3),((-9),7),((-5),9)]
    renderWithShade (Color3 1.0 0.45 0) (Color3 0.4 0.2 0) $
      renderPrimitive LineStrip $ ugoVertices2D 0 1
        [((-12.0),(3.4)),(0.8,8.7),((-8.1),(-0.9)),(4.0,5.8),(4.3,5.6),
          ((-4.4),(-6.8)),((-4.1),(-6.9)),(8.3,0.8),(9.0,0.6),(2.0,(-7.2))]

  renderGameObject StandardMissile{position=x:+y,velocity=v} = preservingMatrix $ do
    let dir = (phase v) :: Double
    translate2D x y
    rotateZ (dir / pi * 180)
    color (Color3 1.0 0.9 0.5)
    renderPrimitive LineLoop $ ugoVertices2D 0 1 [(0,0),(-7,2),(-7,-2)]
    renderPrimitive LineStrip $ ugoVertexFreq (-11) 0 0 1 1 >> ugoVertexFreq (-17) 0 0 7 1

  renderGameObject StandardRailgun{position=x:+y,velocity=v} =
    preservingMatrix $ do
      let (_,phse)=polar v
      translate2D x y
      rotateZ (phse / pi * 180)
      color (Color3 1.0 0.9 0.5)
      renderPrimitive Lines $ ugoVertices2D 0 1 [(0,0),((-5),0),((-9),0),((-11),0)]

  renderGameObject laser@StandardLaser{position=x:+y,velocity=v} =
    if age laser < 1 then return ()
    else preservingMatrix $ do
      let (_,phs)=polar v
      translate2D x y
      rotateZ (phs / pi * 180)
      color (Color3 0.7 0.9 1.0)
      renderPrimitive Lines $ ugoVertices2D 0 0 [(12,0),(-laserSpeed,0)]

  renderGameObject Shield{position=x:+y, size = r,angle = theta} = preservingMatrix $ do
    translate2D x y
    rotateZ theta
    renderWithShade (Color3 0.375 0.75 0.9375) (Color3 0.86 0.86 0.86) $ do
      scale r r 0
      renderTriangle
      rotateZ 60
      renderTriangle where
        renderTriangle = do
          renderPrimitive LineLoop $ ugoVertices2DFreq 0 0.1 1 $ map (\t -> (cos t,sin t)) [0,pi*2/3,pi*4/3]

  renderGameObject powerUpCapsule@PowerUpCapsule{} = preservingMatrix $ do
    let x:+y = position powerUpCapsule
    translate2D x y
    renderWithShade (Color3 0.9 0.9 0.9) (Color3 0.4 0.4 0.4) $ do
      futa >> neji >> toge >> rotateX (180) >> toge
      rotateY (180)
      futa >> neji >> toge >> rotateX (180) >> toge
    renderWithShade (Color3 1.0 0.0 0.0) (Color3 0.3 0.3 0.0) $ do
      nakami
      where
        r = renderPrimitive LineStrip . ugoVertices2D 0 1
        futa = r [((-10),6),((-6),10),(6,10),(10,6)]
        neji = r [(12,4),(12,(-4))] >> r [(16,2),(16,(-2))]
        toge = r [(10,8),(16,14)]
        nakami = rotate 145 (Vector3 0.2 0.2 1) >> scale 9 6 1 >>
          (renderPrimitive LineStrip $ ugoVertices2D 0 0.2 $ map (\n ->  (cos$n*pi/8,sin$n*pi/8)) [1,15,3,13,5,11,7,9])

  renderGameObject DiamondBomb{position = (x:+y),age=clock} = preservingMatrix $ do
    translate2D x y
    rotateZ (90*intToDouble(clock`mod`4))
    color (Color3 1 1 1)
    renderPrimitive LineLoop $ vertices2D 0 $ [a,b,c]
    color (Color3 0.5 0.5 0.5)
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
    translate2D x y
    color $ Color3 1.0 0.7 1.0
    rotateZ (5 * intToDouble clock)
    renderWing
    rotateZ 120
    renderWing
    rotateZ 120
    renderWing
    where
      renderWing = renderPrimitive LineLoop $ ugoVertices2D 0 2 $ map ((\(t:+u) -> (t,u)) . (\(r,t) -> mkPolar r (pi*t)) )
        [(3,0), (3,2/3), (smallBacterianSize,1/3), (smallBacterianSize,0), (smallBacterianSize+3,-1/3)]

  renderGameObject Flyer{position=x:+y,age=_,velocity = v,hasItem=item}  = preservingMatrix $ do
    translate $ Vector3 x y 0
    color (if item then Color3 1.0 0.2 0.2 else Color3 0.3 1.0 0.7)
    rotateZ (phase v / pi * 180)
    r [(-2,0),(-6,4),(-10,0),(-6,-4)]
    r [(2,4),(16,4),(4,16),(-10,16)]
    r [(2,-4),(16,-4),(4,-16),(-10,-16)]
    where r = renderPrimitive LineLoop . ugoVertices2D 0 2

  renderGameObject Ducker{position = (x:+y),hitDisp=hd,hasItem=item,velocity = v,gVelocity = g,age = a} = preservingMatrix $ do
    translate2D x y
    if signum (imagPart g) > 0 then scale 1 (-1) 1 else return ()
    if signum (realPart v) < 0 then scale (-1) 1 1 else return ()
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
    translate2D x y
    color (if item then Color3 1.0 0.2 0.2 else Color3 0.3 1.0 0.7)
    renderShape (0:+0) hd
    if gsign >0 then rotateX 180 else return() -- after this you can assume that the object is not upside down
    renderPrimitive LineStrip $ ugoVertices2D 0 2 $ [(15,-5),(25,-5+absvy*leg),(25,-25+absvy*leg)]
    renderPrimitive LineStrip $ ugoVertices2D 0 2 $ [(-15,-5),(-25,-5+absvy*leg),(-25,-25+absvy*leg)]
    where
      gsign = signum $ imagPart g
      absvy = imagPart v * gsign -- if falling (+) ascending (-)
      leg = 1.5

  renderGameObject Grashia{position = (x:+y),hitDisp=hd,hasItem=item,gunVector = nv,gravity = g,mode=m} = preservingMatrix $ do
    color (if item then Color3 1.0 0.2 0.2 else Color3 0.3 1.0 0.7)
    translate2D x y
    renderShape (0:+0) hd
    renderPrimitive LineLoop $ ugoVertices2D 0 2 $ map (\r -> (nvx*r,nvy*r)) [16,32]
    when (m == 1) $ do
      renderShape 0 $ Circular (16:+12*gsign) 4
      renderShape 0 $ Circular ((-16):+12*gsign) 4
    where
      nvx:+nvy = nv
      gsign = signum $ imagPart g

  renderGameObject me@ScrambleHatch{position = (x:+y),hitDisp=_,gravity= g,gateAngle = angl} = preservingMatrix $ do
    translate2D x y
    color $ Color3 (1.2*(1-hpRate)) 0.5 (1.6*hpRate)
    if gsign >0 then rotateX 180 else return() -- after this you can assume that the object is not upside down
    renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*2) 1 $ [(-45,1),(-45,hatchHeight),(45,hatchHeight),(45,1)]
    preservingMatrix $ do
      translate (Vector3 45 hatchHeight 0)
      rotateZ (-angl/pi*180)
      renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*1) 2 $ [(0,0),(-45,0),(-45,10)]
    preservingMatrix $ do
      translate (Vector3 (-45) hatchHeight 0)
      rotateZ (angl/pi*180)
      renderPrimitive LineLoop $ ugoVertices2DFreq 0 (angl*1) 2 $ [(0,0),(45,0),(45,10)]
    where
      gsign = signum $ imagPart g
      hpRate = (intToDouble $ hp me)/(intToDouble hatchHP)

  renderGameObject LandScapeBlock{position=pos,hitDisp=hd} = preservingMatrix $ do
    color $ Color3 0.6 0.2 0
    renderShape pos hd
    when (treasure!!(baseGameLevel variables)) $ do
      render (Color3 0.7 0.23 0) (Vector3 0 0 60)
      render (Color3 0.5 0.17 0) (Vector3 0 0 (-120))
    where
      render col trans = color col >> translate trans >> renderShape pos hd

  renderGameObject me@Particle{position = x:+y,particleColor=pc} = preservingMatrix $ do
    when (age me>=0) $ do
      translate2D x y
      color $ modifyColor (\e -> e * decay + whiteout) pc
      renderShape (0:+0) $ Circular (0:+0) (size me*extent)
    where
      p n = intCut $ (*n) $ intToDouble (age me) / decayTime me
      extent = 0.5 + p 1
      decay =  exp $ p (-1)
      whiteout = exp $ p (-2)
      intCut :: Double -> Double
      intCut = intToDouble.round

  renderGameObject Star{position = x:+y,particleColor=c} = preservingMatrix $ do
    color c
    renderPrimitive LineStrip $ ugoVertices2D 0 2 [(0.1+x,0+y),(-0.1+x,0+y)]


  renderGameObject DebugMessage{debugMessage=str} =
    putDebugStrLn str

  renderGameObject _ = return ()

  vicViper = head $ filter (\obj -> case obj of
                              VicViper{} -> True
                              _          -> False) objects

  renderShape :: Complex Double -> Shape -> IO ()
  renderShape (x:+y) s = case s of
    Rectangular{bottomLeft = (l:+b), topRight = (r:+t)}  ->
      renderPrimitive LineLoop $ vertices2D 0 [(x+l,y+b),(x+l,y+t),(x+r,y+t),(x+r,y+b)]
    Circular{center=cx:+cy, radius = r} -> preservingMatrix $ do
      translate (Vector3 (cx+x) (cy+y) 0)
      rotateZ (intToDouble gameclock*(45+pi))
      scale r r 1
      renderPrimitive LineLoop $ vertices2D 0 $ map (\t -> (cos(2/7*t*pi),sin(2/7*t*pi))) [0..6]
    Shapes{children=cs} -> mapM_ (renderShape (x:+y)) cs



  -- renders a vertex at somewhere near (x y z),
  -- but the point wiggles around in ugoRange when each interval comes.
  ugoVertexFreq x y z = ugoFreq f x y z
    where f (x',y',z') = vertex $ Vertex3 x' y' z'

  ugoTranslate x y z ugoRange = ugoTranslateFreq x y z ugoRange standardUgoInterval
  ugoTranslateFreq x y z = ugoFreq f x y z
    where f (x',y',z') = translate $ Vector3 x' y' z'

  ugoFreq f x y z ugoRange interval = f (x+dr*cos theta, y+dr*sin theta, z)
    where
      flipper :: Double
      flipper = fromIntegral $ (gameclock `div` interval) `mod` 1024
      dr = ugoRange * vibrator(phi)
      theta = (x + sqrt(2)*y + sqrt(3)*z + 573) * 400 * flipper
      phi   = (x + sqrt(3)*y + sqrt(7)*z + 106) * 150 * flipper
      vibrator a = 0.5 * (1 + sin a)

  ugoVertices2D z r xys = ugoVertices2DFreq z r standardUgoInterval xys
  ugoVertices2DFreq z r intrvl xys = mapM_ (\(x,y) -> ugoVertexFreq x y z r intrvl) xys

vertices2D :: Double -> [(Double,Double)] -> IO ()
vertices2D z xys = mapM_ (\(x,y) -> vertex $ Vertex3 x y z) xys

