module Stage ( loadObjects ) where

import GameObject
import Data.Complex 
import Data.Array

-- loadObjects :: [GameObject]
-- a list of objects that are to newly loaded at this frame.

loadObjects clock isEasy vicViper gameObjects = if hp vicViper<=0 then [] else (case clock of
    -- stage layout.
    -- just like old BASIC code.
    150 -> [freshTurnGearSquad{position=340:+(180)}]
    300 -> [freshTurnGearSquad{position=340:+(-180)}]
    400 -> [freshTurnGearSquad{position=340:+(180)}]
    500 -> [freshTurnGearSquad{position=340:+(-180)}]
    633 -> map (\y -> freshStalk{position = 340:+y,hasItem=False }) [-120,120] ++ [freshStalk{position = 340:+0,hasItem=isEasy}]
    666 -> map (\y -> freshStalk{position = 340:+y,hasItem=isEasy}) [-130,130] ++ [freshStalk{position = 340:+0,hasItem=False}]
    700 -> map (\y -> freshStalk{position = 340:+y,hasItem=False }) [-140,140] ++ [freshStalk{position = 340:+0,hasItem=True}]
    733 -> map (\y -> freshStalk{position = 340:+y,hasItem=isEasy}) [-150,150] ++ [freshStalk{position = 340:+0,hasItem=False}]
    900 -> [freshTurnGearSquad{position=340:+(-180)}]
    1000 -> [freshTurnGearSquad{position=340:+(180)}]
    1050 -> map (\y -> freshStalk{position = 340:+y}) [-135,0,135]
    1250 -> map (\y -> freshFlyer{position = 340:+y}) [-150,-100]
    1300 -> map (\y -> freshFlyer{position = 340:+y,hasItem=True}) [100,150]
    1100 -> [freshTurnGearSquad{position=340:+(-180)},freshTurnGearSquad{position=340:+(180)}]
    1400 -> [(freshGrashia (-1)){position = 340:+(-185)},(freshGrashia 1){position = 340:+(185)}]
    1450 -> [(freshGrashia (-1)){position = 340:+(-185),hasItem=True},(freshGrashia 1){position = 340:+(185)}]
    1550 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
    1700 -> [(freshVolcano (-1)){position=479:+(-200)}]
    1900 -> map (\(g,x) ->  (freshDucker g){position=x:+g*100}) $ [(1,340),(-1,340)] ++ if not isEasy then [(1,-340),(-1,-340)] else []
    1940 -> [(freshLandRoll (1)){position = 340:+(185)}]
    1965 -> [(freshLandRoll (1)){position = 340:+(185)}]
    1990 -> [(freshLandRoll (1)){position = 340:+(185)}]
    2000 -> [(freshGrashia (-1)){position = 340:+(-185)},(freshDucker (-1)){position=(-340):+(-185)}]
    2033 -> [(freshGrashia (-1)){position = 340:+(-185)},(freshDucker 1){position=(-340):+(185)}]
    2100 -> [(freshScrambleHatch (-1)){position = 360:+(-200)}]
    2200 -> [(freshVolcano 1){position=479:+(200)}]
    2250 -> map (\y -> freshStalk{position = 340:+y}) [-150,0,150]
    2339 -> [(freshGrashia (1)){position = 340:+35},(freshGrashia (-1)){position = 340:+(-185)}]

    2433 -> map (\y -> freshFlyer{position = 340:+y}) [-150,0]
    2466 -> map (\y -> freshFlyer{position = 340:+y}) [-150,0]
    2499 -> map (\y -> freshFlyer{position = 340:+y}) [-150,0]

    2620 -> [(freshDucker 1){position=(-340):+(200)}]
    2640 -> [(freshDucker 1){position=(-340):+(200),hasItem=True}]
    2800 -> map (\(g,x) ->  (freshJumper g){position=x:+g*100,velocity=((-3)*signum x):+0}) $ [(1,340),(-1,340)] ++ if not isEasy then [(1,-340),(-1,-340)] else []
    2999 -> [(freshVolcano 2){position=479:+(20),velocity=(0:+(-0.5))},(freshVolcano (-2)){position=479:+(-20),velocity=(0:+(0.5))}]

    3200 -> concat $ map (\x -> [freshLandScapeGround{position=(479-x):+220},freshLandScapeGround{position=(479-x):+(-220)}]) [320,640]

    3210 -> [freshFlyer{position = 340:+150},freshFlyer{position = 340:+100,hasItem=True}]
    3290 -> [freshFlyer{position = 340:+(-150)},freshFlyer{position = 340:+(-100),hasItem=True}]
    3350 -> map (\g -> (freshLandRoll (g)){position = 340:+(g*185)}) [1,-1] ++ if isRevival then [] else [(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
    3400 -> map (\g -> (freshLandRoll (g)){position = 340:+(g*185)}) [1,-1] ++ if isRevival then [] else [(freshJumper (-1)){position = (-340):+(-150),velocity=3:+0}]
    3450 -> map (\g -> (freshLandRoll (g)){position = 340:+(g*185)}) [1,-1]
    3500 -> [(freshVolcano (-1)){position=479:+(-200)}]
    3579 -> [(freshGrashia (-1)){position = 340:+(-100)}]
    3639 -> [(freshGrashia (-1)){position = 340:+(-40)}]
    3699 -> [(freshGrashia (-1)){position = 340:+(-100)}]
    3501 -> [(freshScrambleHatch (1)){position = 360:+(200)}]
    3600 -> [(freshScrambleHatch (1)){position = 360:+(200)}]
    3582 -> [(freshDucker (-1)){position=(340):+(-200)}]
    3612 -> [(freshDucker (-1)){position=(340):+(-200)}]
    3642 -> [(freshDucker (-1)){position=(340):+(-200)}]
    3672 -> [(freshDucker (-1)){position=(340):+(-200)}]
    3702 -> [(freshDucker (-1)){position=(340):+(-200),hasItem=True}]
    3703 -> [(freshLandRoll (1)){position=(340):+(185),hasItem=True}]
    3820 -> map (\y -> freshFlyer{position = 340:+y}) [-100,100]
    3840 -> map (\y -> freshFlyer{position = 340:+y}) [-110,110]
    3860 -> map (\y -> freshFlyer{position = 340:+y}) [-120,120]
    3880 -> map (\y -> freshFlyer{position = 340:+y,hasItem = isEasy}) [-130,130]
    3900 -> [freshTurnGearSquad{position=340:+0}]
    4000 -> [(freshTable 1){position=450:+200}]
    4033 -> [(freshGrashia (1)){position = 340:+(185)}]
    4066 -> [(freshGrashia (1)){position = 340:+(185)}]
    4060 -> [(freshGrashia (1)){position = 340:+(40)}]
    4110 -> [(freshGrashia (1)){position = 340:+(40)}]
    4160 -> [(freshGrashia (1)){position = 340:+(40)}]
    4166 -> [(freshGrashia (1)){position = 340:+(185),hasItem=True}]
    4200 -> [(freshGrashia (1)){position = 340:+(185),hasItem=True}]
    4233 -> [(freshGrashia (1)){position = 340:+(185),hasItem=False}]
    4266 -> [(freshGrashia (1)){position = 340:+(185),hasItem=False}]
    4150 -> [freshLandScapeGround{position=479:+(-180)}]
    4203 -> [(freshJumper (-1)){position = 340:+(-180)}]
    4273 -> [(freshJumper (-1)){position = 340:+(-180)}]
    4343 -> [(freshJumper (-1)){position = 340:+(-180)}]
    4490 -> [(freshTable (-1)){position=450:+(-200)}]
    4500 -> [(freshDucker (-1)){position=340:+0}]
    4520 -> [(freshDucker (-1)){position=340:+0}]
    4540 -> [(freshDucker (-1)){position=340:+0}]
    4560 -> [(freshGrashia (-1)){position = 340:+(-185)}]
    4580 -> [(freshScrambleHatch (-1)){position = 360:+(-50)}]
    4603 -> if isRevival then [] else [(freshDucker 1){position = (-340):+0},(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
    4663 -> [(freshDucker 1){position = (-340):+0}]++if isEasy then [] else [(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
    4723 -> if isRevival then [] else [(freshDucker 1){position = (-340):+0},(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
    4783 -> [(freshDucker 1){position = (-340):+0}]++if isEasy then [] else [(freshJumper (1)){position = (-340):+150,velocity=3:+0}]
    4680 -> [(freshScrambleHatch (-1)){position = 360:+(-200)}]
    4900 -> map (\y -> freshFlyer{position = 340:+y}) [-100,100]
    4930 -> map (\y -> freshFlyer{position = 340:+y}) [-66,66]
    4960 -> map (\y -> freshFlyer{position = 340:+y}) [-33,33]
    4990 -> map (\y -> freshFlyer{position = 340:+y,hasItem=True}) [0]
    5041 -> [(freshDucker (-1)){position = 340:+(-180)}]
    5061 -> [(freshDucker (-1)){position = 340:+(-180)}]
    5081 -> [(freshDucker (-1)){position = 340:+(-180)}]
    5101 -> if isRevival then [] else ([(freshDucker (-1)){position = 340:+(-180)}] ++ if isEasy then [] else [(freshDucker (1)){position = (-340):+(180)}])
    5121 -> if isRevival then [] else ([(freshDucker (-1)){position = 340:+(-180)}] ++ if isEasy then [] else [(freshDucker (1)){position = (-340):+(180)}])
    5141 -> if isRevival then [] else ([(freshDucker (-1)){position = 340:+(-180)}] ++ if isEasy then [] else [(freshDucker (1)){position = (-340):+(180)}])
    5261 -> [freshTurnGearSquad{position=340:+(-150)}]
    5364 -> [(freshScrambleHatch (-1)){position = 360:+(-200)}]
    5151 -> [(freshDucker (-1)){position = (-340):+(0)}]
    5181 -> [(freshDucker (-1)){position = (-340):+(0)}]
    5211 -> [(freshDucker (-1)){position = (-340):+(0)}]
    5241 -> [(freshDucker (-1)){position = (-340):+(0)}]
    5321 -> [(freshDucker (-1)){position = 340:+150}] ++ if isEasy then [] else [(freshJumper (1)){position = (-340):+(180),velocity = 3:+0}]
    5361 -> [(freshDucker (-1)){position = 340:+150}]
    5401 -> [(freshDucker (-1)){position = 340:+150}]
    5441 -> [(freshDucker (-1)){position = 340:+150}] ++ if isEasy then [] else map (\y -> freshStalk{position = 340:+y}) [-140,-70,0]
    5461 -> [(freshDucker (-1)){position = 340:+150}]
    5451 -> [(freshGrashia (-1)){position = 340:+160,hasItem=True}]

    5060 -> [(freshVolcano (-1)){position = 480:+(-100)}]
    5200 -> [(freshGrashia (-1)){position = 340:+70}] ++ [(freshDucker (-1)){position = 340:+70}]
    5235 -> [(freshGrashia (-1)){position = 340:+40}] ++ [(freshDucker (-1)){position = 340:+40}]
    5258 -> [(freshGrashia (-1)){position = 340:+10}] ++ [(freshDucker (-1)){position = 340:+10}]
    5285 -> [(freshGrashia (-1)){position = 340:+(-20)}] ++ [(freshDucker (-1)){position = 340:+(-20)}]
    5316 -> [(freshGrashia (-1)){position = 340:+(-50)}] ++ [(freshDucker (-1)){position = 340:+(-50)}]

    5310 -> [(freshVolcano (1)){position = 480:+(150)}]
    5450 -> [(freshGrashia (1)){position = 340:+(-20)}]
    5485 -> [(freshGrashia (1)){position = 340:+10}]
    5508 -> [(freshGrashia (1)){position = 340:+40}]
    5535 -> [(freshGrashia (1)){position = 340:+70}]
    5566 -> [(freshGrashia (1)){position = 340:+100}]

    5811 -> [(freshDucker (-1)){position = (-340):+0}]
    5841 -> [(freshDucker (-1)){position = (-340):+0}]
    5871 -> [(freshDucker (-1)){position = (-340):+0}]
    5901 -> [(freshDucker (-1)){position = (-340):+0}]
    6001 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []
    6031 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []
    6061 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []
    6091 -> if isEasy then [(freshDucker (-1)){position = (-340):+150}] else []

    5800 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
    5950 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
    6100 -> [(freshScrambleHatch (-1)){position = 360:+(-200)},(freshScrambleHatch (1)){position = 360:+(200)}]
    6116 -> [freshSabbathicAgent]

    _ -> []) ++
  (if(optionCount < powerUpLevels vicViper!4) then
    [freshOption{position=position vicViper, optionTag = optionCount+1}]
    else []) ++
  (if (clock `mod` 320 == 0 && clock>=1280 && clock <= 6400) then
    [freshLandScapeGround{position=479:+220},freshLandScapeGround{position=479:+(-220)}]
    else [])
  where
    optionCount = length $ filter (\o -> case o of
      Option{} -> True
      _  -> False) gameObjects
    isRevival = optionCount <= 0
