module Input (handleInputIO) where

import AI
import Board
import Control.Concurrent
import Control.Exception
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

-- functions for snapping
coordSnap w coord = rndAdv 10 (round $ tSize w) coord

clickSnap :: World -> (Integer, Integer) -> (Integer, Integer)
clickSnap w (xCoord, yCoord) = (coordSnap w $ toInteger xCoord, coordSnap w $ toInteger yCoord)

rndAdv :: Int -> Integer -> Integer -> Integer
rndAdv size target input = do
  if even size
    then do
      if input >= 0
        then rnd target input
        else (-1) * (rnd target (input * (-1)))
    else do
      if input >= 0
        then rnd target (input + 25) - 25
        else (-1) * (rnd target ((-input + 25) - 25))

rnd :: Integer -> Integer -> Integer
rnd target input = do
  let temp = rem input target
  if temp < div target 2
    then input - temp
    else input + 50 - temp

first (a, b) = a

second (a, b) = b

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!

handleInputIO :: Event -> World -> IO World
handleInputIO (EventKey (Char 's') Up _ _) w =
  trace "stepping" $ return w
handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) w = trace ("Click on: " ++ show (x, y) ++ "Snap to: " ++ show (clickSnap w (round x, round y))) $ return w
handleInputIO e b = return b
