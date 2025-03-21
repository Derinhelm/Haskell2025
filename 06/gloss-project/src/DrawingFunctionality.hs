module DrawingFunctionality(
    windowWidth,
    windowHeight,
    GameState (..),
    defaultGameState,
    drawScene,
    timeTick,
    incSimulationSpeed,
    decSimulationSpeed,
    incGrowthSpeed,
    decGrowthSpeed,
    pause,
    changeMode,
    changeFlowersState,
    clear
    )
    where

--------------------------------------------------------------------------
------------------------------ Libraries ---------------------------------
--------------------------------------------------------------------------
import Graphics.Gloss.Interface.Pure.Game
--------------------------------------------------------------------------
---------------------------- Project files -------------------------------
--------------------------------------------------------------------------
import FractalTree
--------------------------------------------------------------------------
----------------------------- Data types ---------------------------------
--------------------------------------------------------------------------
data GameState = GameState 
    { tree           :: FractalTree,
      time           :: Int,
      timeSpeed      :: Int,
      rootTreeGrowth :: Int,
      mode           :: Int,
      sakura         :: Bool
    } deriving Show

-- in minuters => max = 1440
newtype Time = Time Int

-- colors in RGBA format with values from [0, 1]
data DayColors = DayColors Float Float Float Float

data DayPart = Nigth | Sunrise | Day | Sunset | Twilight deriving (Show)

-- Configuration file for drawing
data DrawingParameters = DrawingParameters{
    trunkColor          :: Color,
    leavesColor         :: Color,
    trunkToLeavesLength :: Float,
    flowersColorIn      :: Color,
    flowersColorOut     :: Color
}

--------------------------------------------------------------------------
------------------------------ Constants ---------------------------------
--------------------------------------------------------------------------
windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 800

-- initial world
defaultGameState :: GameState
defaultGameState = GameState {  tree           = computeTree defaultTree,
                                time           = 1,
                                timeSpeed      = 1,
                                rootTreeGrowth = 10,
                                mode           = 1,
                                sakura         = True
                            }

-- Should be read from file!!!
-- Distribution of day colors through day
skyColorsByDayTime :: [(DayColors, Time, DayPart)]
skyColorsByDayTime = [
    -- nigth
    (DayColors         0         0         0 1,   Time 0,          Nigth), -- midnigth
    -- sunrise
    (DayColors ( 94/255) ( 44/255) (111/255) 0.6, Time $ 3*60,     Sunrise),  -- 3.00
    (DayColors (231/255) ( 76/255) ( 60/255) 1,   Time $ 4*60+15,  Sunrise),  -- 4.15
    (DayColors (243/255) (156/255) ( 18/255) 1,   Time $ 4*60+30,  Sunrise),  -- 4.30
    (DayColors (245/255) (176/255) ( 65/255) 1,   Time $ 4*60+45,  Sunrise),  -- 4.45
    (DayColors (247/255) (220/255) ( 11/255) 1,   Time $ 5*60,     Sunrise),  -- 5.00
    -- midday
    (DayColors (107/255) (207/255) (255/255) 1,   Time $ 5*60+30,  Day),      -- 5.30
    (DayColors ( 75/255) (197/255) (255/255) 1,   Time $ 6*60,     Day),      -- 6.00
    (DayColors ( 51/255) (190/255) (255/255) 1,   Time $ 9*60,     Day),      -- 9.00
    (DayColors ( 39/255) (185/255) (255/255) 1,   Time $ 11*60,    Day),      -- 11.00
    (DayColors (  0/255) (173/255) (255/255) 1,   Time $ 12*60,    Day),      -- 12.00
    (DayColors ( 39/255) (185/255) (255/255) 1,   Time $ 13*60,    Day),      -- 13.00
    (DayColors ( 51/255) (190/255) (255/255) 1,   Time $ 15*60,    Day),      -- 15.00
    (DayColors ( 75/255) (197/255) (255/255) 1,   Time $ 17*60,    Day),      -- 17.00
    (DayColors (107/255) (207/255) (255/255) 1,   Time $ 18*60,    Day),      -- 18.00
    -- sunset
    (DayColors (255/255) (170/255) ( 32/255) 1,   Time $ 18*60+30, Sunset),   -- 18.30
    (DayColors (239/255) (101/255) (  0/255) 1,   Time $ 19*60+15, Sunset),   -- 19.15
    (DayColors (248/255) (  1/255) (127/255) 1,   Time $ 19*60+30, Sunset),   -- 19.30
    (DayColors (183/255) ( 56/255) ( 80/255) 1,   Time $ 19*60+45, Sunset),   -- 19.45
    (DayColors (119/255) (  1/255) (141/255) 0.6, Time $ 20*60,    Twilight), -- 20.00
    -- after twilight
    (DayColors ( 63/255) ( 30/255) (103/255) 1,   Time $ 21*60,    Twilight), -- 21.00
    (DayColors ( 37/255) (  6/255) ( 76/255) 1,   Time $ 22*60,    Twilight), -- 22.00
    (DayColors (  8/255) (  4/255) ( 15/255) 1,   Time $ 23*60,    Nigth),    -- 23.00


    (DayColors         0         0         0 1,    Time   1440,    Nigth)     -- midnigth
    ]

-- makeColorI :: Int == Red component
--            -> Int == Green component
--            -> Int == Blue component
--            -> Int == Alpha component
--            -> Color 
defaultDrawingParameters :: DrawingParameters
defaultDrawingParameters = DrawingParameters{
    trunkColor          = (makeColorI 165 42 42 255),
    leavesColor         = (makeColorI 34 139 34 255),
    trunkToLeavesLength = 30,
    flowersColorOut     = (makeColor (255/255) (128/255) (128/255) 1),
    flowersColorIn      = (makeColor (255/255) (198/255) (198/255) 1)
}

--------------------------------------------------------------------------
------------------ Changing game world Functions -------------------------
--------------------------------------------------------------------------
-- Changing global time in simulation
timeTick :: GameState -> GameState
timeTick state  = GameState {   tree           = (tree state),
                                time           = (time state) + (timeSpeed state),
                                timeSpeed      = (timeSpeed state),
                                rootTreeGrowth = (rootTreeGrowth state),
                                mode           = (mode state),
                                sakura         = (sakura state)
                            }

-- Increasing global time depending on speed simulation
incSimulationSpeed :: GameState -> GameState
incSimulationSpeed state | (timeSpeed state) < 10 = GameState { tree           = (tree state),
                                                                time           = (time state),
                                                                timeSpeed      = (timeSpeed state) + 1,
                                                                rootTreeGrowth = (rootTreeGrowth state),
                                                                mode           = (mode state),
                                                                sakura         = (sakura state)
                                                              }
                         | otherwise = state

-- Decreasing global time depending on speed simulation
decSimulationSpeed :: GameState -> GameState
decSimulationSpeed state | (timeSpeed state) > 1  = GameState { tree           = (tree state),
                                                                time           = (time state),
                                                                timeSpeed      = (timeSpeed state) - 1,
                                                                rootTreeGrowth = (rootTreeGrowth state),
                                                                mode           = (mode state),
                                                                sakura         = (sakura state)
                                                              }
                         | otherwise = state

-- Function that increases speed with which FromRoot grows
incGrowthSpeed :: GameState -> GameState
incGrowthSpeed state | (rootTreeGrowth state) > 1 = GameState { tree           = (tree s),
                                                                time           = (time s),
                                                                timeSpeed      = (timeSpeed s),
                                                                rootTreeGrowth = prevSpeed - 1,
                                                                mode           = (mode s),
                                                                sakura         = (sakura s)
                                                              }
                     | otherwise = state
                        where 
                            prevSpeed = (rootTreeGrowth state)
                            s = clear state

-- Function that decreases speed with which FromRoot grows
decGrowthSpeed :: GameState -> GameState
decGrowthSpeed state | (rootTreeGrowth state) < 20 = GameState { tree          = (tree s),
                                                                time           = (time s),
                                                                timeSpeed      = (timeSpeed s),
                                                                rootTreeGrowth = prevSpeed + 1,
                                                                mode           = (mode s),
                                                                sakura         = (sakura s)
                                                              }
                    | otherwise = state
                        where 
                            prevSpeed = (rootTreeGrowth state)
                            s = clear state

-- Pauses or resemble simulation be changing speed to 0 or 1 
pause :: GameState -> GameState
pause state | (timeSpeed state) == 0  = GameState {tree           = (tree state),
                                                   time           = (time state),
                                                   timeSpeed      = 1,
                                                   rootTreeGrowth = (rootTreeGrowth state),
                                                   mode           = (mode state),
                                                   sakura         = (sakura state)
                                                  }
            | otherwise               = GameState {tree           = (tree state),
                                                   time           = (time state),
                                                   timeSpeed      = 0,
                                                   rootTreeGrowth = (rootTreeGrowth state),
                                                   mode           = (mode state),
                                                   sakura         = (sakura state)
                                                  }

-- Changing from Simple tree to FromRoot or vice versa
changeMode :: GameState -> Int -> GameState
changeMode state newMode | (newMode == 1)  = GameState {tree           = computeTree defaultTree,
                                                        time           = (time state),
                                                        timeSpeed      = (timeSpeed state),
                                                        rootTreeGrowth = (rootTreeGrowth state),
                                                        mode           = newMode,
                                                        sakura         = (sakura state)
                                                        }
                         | (newMode == 2)  = GameState {tree           = computeTree defaultTreeRoot,
                                                        time           = (time state),
                                                        timeSpeed      = (timeSpeed state),
                                                        rootTreeGrowth = (rootTreeGrowth state),
                                                        mode           = newMode,
                                                        sakura         = (sakura state)
                                                        }
                         | otherwise = state 

-- Changes flowers state (draw or not draw flowers)
changeFlowersState :: GameState -> (Float, Float) -> GameState
changeFlowersState state (x, y) | y < 0 = GameState { tree           = (tree s),
                                                      time           = (time s),
                                                      timeSpeed      = (timeSpeed s),
                                                      rootTreeGrowth = (rootTreeGrowth s),
                                                      mode           = (mode s),
                                                      sakura         = not (sakura s)
                                                    }
                                | otherwise = state
                                    where s = case (x > 0) of
                                                True -> clear state
                                                False -> state

-- Renews simulation
clear :: GameState -> GameState
clear state = GameState {   tree           = computeTree tree,
                            time           = 1,
                            timeSpeed      = 1,
                            rootTreeGrowth = 10,
                            mode           = (mode state),
                            sakura         = (sakura state)
                        }
                        where 
                            tree = case (mode state) of
                                    1 -> defaultTree
                                    2 -> defaultTreeRoot
                                    _ -> defaultTree
--------------------------------------------------------------------------
--------------------------- Draw Functions -------------------------------
--------------------------------------------------------------------------
-- Function that collects all scene objects and combines them.
drawScene :: GameState -> Picture
drawScene state | (sakura state) = Pictures [(drawSky (time state)),
                                             (info state),
                                             (drawTree (tree state) (time state) (rootTreeGrowth state)),
                                             (drawFlowers (tree state) (time state) (sakuraLeaf defaultSakuraLeaf) (rootTreeGrowth state))
                                            ]
                | otherwise = Pictures [(drawSky (time state)),
                                         (info state),
                                         (drawTree (tree state) (time state) (rootTreeGrowth state))
                                        ]

------------------------------ Draw Info ---------------------------------
info :: GameState -> Picture
info state  = Pictures [(timeInfo (time state)), (dayPartInfo (time state)), (speedInfo state (time state))]

timeInfo :: Int -> Picture
timeInfo k =  Translate timeXPos timeYPos $ Scale 0.2 0.2 $ Color col $ Text $ "Time: " ++ hourStr ++ ":" ++ minStr
                where
                    timeXPos = (fromIntegral (-windowWidth)) / 2 + 35
                    timeYPos = (fromIntegral (windowHeight)) / 2 - 40
                    col | (k `mod` 1440 < 3*60) || (k `mod` 1440 > 21*60) = white
                        | otherwise = black
                    hours = (k `div` 60) `mod` 24
                    minuters = k `mod` 60
                    hourStr | hours < 10 = "0"++(show hours)
                            | otherwise = show hours
                    minStr  | minuters < 10 = "0"++(show minuters)
                            | otherwise = show minuters

dayPartInfo :: Int -> Picture
dayPartInfo k = Translate timeXPos timeYPos $ Scale 0.2 0.2 $ Color col $ Text $ "Day part: " ++ daypart  
                where
                    timeXPos = (fromIntegral (-windowWidth)) / 2 + 35 + 180
                    timeYPos = (fromIntegral (windowHeight)) / 2 - 40
                    daypart = show $ parseDayPart (k `mod` 1440) skyColorsByDayTime
                    col | (k `mod` 1440 < 3*60) || (k `mod` 1440 > 21*60) = white
                        | otherwise = black

parseDayPart :: Int -> [(DayColors, Time, DayPart)] -> DayPart
parseDayPart c ((_, (Time t1), dp1):(col, (Time t2), dp2):xs) | (c >= t1) && (c <= t2) = dp1  
                                                              | otherwise = parseDayPart c ((col, (Time t2), dp2):xs)

speedInfo :: GameState -> Int -> Picture
speedInfo state k = Translate speedXPos speedYPos $ Scale 0.2 0.2 $ Color col $ Text $ "Simulation speed: " ++ speedInfoStr  ++ "x" 
                    where
                        speedXPos = (fromIntegral (-windowWidth)) / 2 + 35
                        speedYPos = (fromIntegral (windowHeight)) / 2 - 40 - 40
                        speedInfoStr = show $ (timeSpeed state)
                        col | (k `mod` 1440 < 3*60) || (k `mod` 1440 > 21*60) = white
                            | otherwise = black

------------------------------ Draw Sky ---------------------------------
drawSky :: Int -> Picture
drawSky k = Color col (polygon [(-w, -h), (w, -h), (w, h), (-w, h), (-w, -h)] )
            where 
                w = fromIntegral windowWidth
                h = fromIntegral windowHeight
                col = skyColor k

skyColor :: Int -> Color
skyColor k = countColorComponent $ parseColorsTime (k `mod` 1440) skyColorsByDayTime

-- (c-t1) == steps
parseColorsTime :: Int -> [(DayColors, Time, DayPart)] -> (Int, (Float, Float, Float, Float), (Float, Float, Float, Float), Int)
parseColorsTime c ((DayColors r1 g1 b1 a1, Time t1, _):(DayColors r2 g2 b2 a2, Time t2, dp):xs)
    | (c >= t1) && (c <= t2) = (step, dayCol1, dayCol2, time)
    | otherwise = parseColorsTime c ((DayColors r2 g2 b2 a2, Time t2, dp):xs)
        where 
            step = c - t1
            dayCol1 = (r1, g1, b1, a1)
            dayCol2 = (r2, g2, b2, a2)
            time = t2 - t1

-- count RGB parameter according to time passed
countColorComponent :: (Int, (Float, Float, Float, Float), (Float, Float, Float, Float), Int) -> Color
countColorComponent (step, (r1, g1, b1, a1), (r2, g2, b2, a2), time) = makeColor r3 g3 b3 a3
                                            where 
                                                kR = (r2 - r1) / (fromIntegral time)
                                                kG = (g2 - g1) / (fromIntegral time)
                                                kB = (b2 - b1) / (fromIntegral time)
                                                kA = (a2 - a1) / (fromIntegral time)
                                                r3 = r1 + (fromIntegral step) * kR
                                                g3 = g1 + (fromIntegral step) * kG
                                                b3 = b1 + (fromIntegral step) * kB
                                                a3 = a1 + (fromIntegral step) * kA

------------------------------ Draw Tree ---------------------------------
drawTree :: FractalTree -> Int -> Int -> Picture
drawTree (Simple t) k _ = drawSimpleTree (Simple t) k
drawTree (FromRoot t) k growthSpeed = drawFromRootTree (FromRoot t) k growthSpeed


drawSimpleTree :: FractalTree -> Int -> Picture
drawSimpleTree (Simple t) k = Pictures (take k $ map prepareLine t)
    where
        yMove = (fromIntegral windowHeight)/2
        len = trunkToLeavesLength defaultDrawingParameters
        trCl = trunkColor defaultDrawingParameters
        lvCl = leavesColor defaultDrawingParameters
        prepareLine = (\br -> case (branchLength br) > len of 
                                    True -> Color trCl (Line $ moveYCoord yMove $ branchToCoords br)
                                    False -> Color lvCl (Line $ moveYCoord yMove $ branchToCoords br))


drawFromRootTree :: FractalTree -> Int -> Int -> Picture
drawFromRootTree (FromRoot t) k growthSpeed = Pictures (map prepareLine $ concat $ take (k `div` growthSpeed) t )
    where
        yMove = (fromIntegral windowHeight)/2
        len = trunkToLeavesLength defaultDrawingParameters
        trCl = trunkColor defaultDrawingParameters
        lvCl = leavesColor defaultDrawingParameters
        prepareLine = (\br -> case (branchLength br) > len of 
                                    True -> Color trCl (Line $ moveYCoord yMove $ branchToCoords br)
                                    False -> Color lvCl (Line $ moveYCoord yMove $ branchToCoords br))

------------------------------ Draw Flowers ---------------------------------
drawFlowers :: FractalTree -> Int -> ((Float, Float) -> SakuraLeaf) -> Int -> Picture
drawFlowers (Simple tree) k leafFunc _ = Pictures $ map (\(x, y) -> Translate x y $ drawSakuraFlower leafFunc) filteredBranches
                                        where 
                                            yMove = (fromIntegral windowHeight)/2
                                            filteredBranches =  map ( head . moveYCoord yMove . branchToCoords) $ filter (\br -> (branchLength br < 5)) $ (take k tree)
drawFlowers (FromRoot tree) k leafFunc growthSpeed| (k `div` growthSpeed) >= length tree = Pictures $ map  (\(x, y) -> Translate x y $ drawSakuraFlower leafFunc) filteredBranches
                                        | otherwise = Blank
                                        where 
                                            yMove = (fromIntegral windowHeight)/2
                                            filteredBranches =  map ( head . moveYCoord yMove . branchToCoords) $ head $ reverse tree


drawSakuraFlower :: ((Float, Float) -> SakuraLeaf) -> Picture
drawSakuraFlower fl  = Pictures [
                                
                                Color cl1 $l, 
                                Color cl1 $ Rotate (72*1) l,
                                Color cl1 $ Rotate (72*2) l,
                                Color cl1 $ Rotate (72*3) l,
                                Color cl1 $ Rotate (72*4) l,
                                Color cl2 $ Scale zoom zoom l,
                                Color cl2 $ Scale zoom zoom $ Rotate (72*1) l,
                                Color cl2 $ Scale zoom zoom $ Rotate (72*2) l,
                                Color cl2 $ Scale zoom zoom $ Rotate (72*3) l,
                                Color cl2 $ Scale zoom zoom $ Rotate (72*4) l
                                ]
                where 
                    l = drawSakuraLeaf fl 
                    zoom = 0.5
                    cl1 = (flowersColorOut defaultDrawingParameters) 
                    cl2 = (flowersColorIn defaultDrawingParameters) 

drawSakuraLeaf :: ((Float, Float) -> SakuraLeaf)   -> Picture
drawSakuraLeaf sakuraLeaf = (polygon $  polygonCoords)
                        where 
                            leaf = (\(SakuraLeaf l) -> l) (sakuraLeaf (0, 0))
                            polygonCoords = map pointToCoords leaf


moveYCoord :: Float -> [(Float, Float)] -> [(Float, Float)]
moveYCoord z [(x1, y1), (x2, y2)] = [(x1, y1-z), (x2, y2-z)]
moveYCoord _ br = br


