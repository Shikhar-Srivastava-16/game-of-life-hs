module Input (handleInputIO) where

import Control.Concurrent
import Control.Exception
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe
import Update
import World

-- functions for snapping
coordSnap w = rndAdv (boardSize w) (round $ tSize w)

clickSnap :: World -> (Integer, Integer) -> (Float, Float)
clickSnap w (xCoord, yCoord) = (fromIntegral $ coordSnap w $ toInteger xCoord, fromIntegral $ coordSnap w $ toInteger yCoord)

rndAdv :: Int -> Integer -> Integer -> Integer
rndAdv size target input = do
  if even size
    then do
      if input >= 0
        then rnd target input
        else (-1) * rnd target (input * (-1))
    else do
      if input >= 0
        then rnd target (input + (target `div` 2)) - (target `div` 2)
        else (-1) * rnd target ((-input + (target `div` 2)) - (target `div` 2))

rnd :: Integer -> Integer -> Integer
rnd target input = do
  let temp = rem input target
  if temp < div target 2
    then input - temp
    else input + target - temp

first (a, b) = a

second (a, b) = b

onClick :: World -> (Float, Float) -> World
onClick w pt = do
  let lists = moveElement pt (wSquares w) (bSquares w)
  w {wSquares = first lists, bSquares = second lists}

handleInputIO :: Event -> World -> IO World
handleInputIO (EventKey (Char 'p') Up _ _) w =
  if state w == Stopped
    then trace "changing to started" $ return $ w {state = Started}
    else trace "changing to stopped" $ return $ w {state = Stopped}
handleInputIO (EventKey (Char 's') Up _ _) w =
  if state w == Started
    then return w
    else return $ step w
handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) w = if state w == Started
  then return w 
  else do
    let snapped = clickSnap w (round x, round y)
    trace ("Click on: " ++ show (x, y) ++ "Snap to: " ++ show snapped) $ return $ onClick w snapped
handleInputIO e w = return w
