module Lib
    (  run
    ) where

--------------------------------------------------------------------------
------------------------------ Libraries ---------------------------------
--------------------------------------------------------------------------
import Graphics.Gloss.Interface.Pure.Game
--------------------------------------------------------------------------
---------------------------- Project files -------------------------------
--------------------------------------------------------------------------
import FractalTree
import DrawingFunctionality

--------------------------------------------------------------------------
----------------------------- Data types ---------------------------------
--------------------------------------------------------------------------

-- Game World is imported from DrawingFunctionality
-- data GameState = GameState 
--     { tree :: FractalTree,
--       time :: Int,
--       timeSpeed :: Int,
--       mode :: Int,
--       sakura :: Bool
--     } deriving Show

--------------------------------------------------------------------------
------------------------------ Constants ---------------------------------
--------------------------------------------------------------------------


-- Display in a window with the given name, size and position.
-- To set fullscreen change InWindow for FullScreen
display :: Display
display = InWindow "Game" (windowWidth, windowHeight) (50, 50)

-- Colors in RGBA
-- To set your color use makeColor::Float->Float->Float->Float->Color or makeColorI::->Int->Int->Int->Int->Color
bgColor :: Color
bgColor = white

-- 	Number of simulation steps to take for each second of real time.
steps :: Int
steps = 10

-- Start game state
initialWorld :: GameState
initialWorld = defaultGameState

--------------------------------------------------------------------------
--------------------------- Pure functions -------------------------------
--------------------------------------------------------------------------

-- Function to transform game world into graphical representation.              
draw :: GameState -> Picture
draw  = drawScene

-- Functions, that recieves events like buttom press of mouse state change
-- and handles such events, namely changes game world correspondably
event :: Event -> GameState -> GameState
event (EventKey (Char 'i') Down _ _) state = incSimulationSpeed state
event (EventKey (Char 'u') Down _ _) state = decSimulationSpeed state
event (EventKey (Char 'p') Down _ _) state = pause state 
event (EventKey (Char 'c') Down _ _) state = clear state 
event (EventKey (Char 'k') Down _ _) state = incGrowthSpeed state
event (EventKey (Char 'j') Up _ _  ) state = decGrowthSpeed state                            
event (EventKey (Char '1') Down _ _) state = changeMode state 1                                                          
event (EventKey (Char '2') Down _ _) state = changeMode state 2                                                           
event (EventKey (MouseButton LeftButton) Down _ pos ) state = changeFlowersState state pos -- handles mouse events 
event _ s = s -- if any other action took place

-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.
update :: Float -> GameState -> GameState
update 0.1 state = timeTick state

--------------------------------------------------------------------------
------------------- Main function for this app ---------------------------
--------------------------------------------------------------------------
run :: IO ()
run = play display bgColor steps initialWorld draw event update