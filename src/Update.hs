module Update where

import Data.Bool
import Debug.Trace
import World

-- Update the world state after some time has passed
{--
   - Live AND (<= Underpopulation OR >=Overpopulation) => Dead
   - Dead AND == Reproduction = 2
   -}
-- toUp (x, y) w = (x, y + tSize w)

-- toLeft (x, y) w = (x - tSize w, y)

-- toRight (x, y) w = (x + tSize w, y)

-- toDown (x, y) w = (x, y - tSize w)

toUp (x, y) w =
  if y + tSize w <= bound w
    then (x, y + tSize w)
    else (x, -(y - tSize w))

-- toLeft (x, y) w = (x - tSize w, y)
toLeft (x, y) w =
  if x - tSize w >= -(bound w)
    then (x - tSize w, y)
    else (-(x + tSize w), y)

toRight (x, y) w =
  if x + tSize w <= bound w
    then (x + tSize w, y)
    else (-(x - tSize w), y)

toDown (x, y) w =
  if y - tSize w >= -(bound w)
    then (x, y - tSize w)
    else (x, -(y + tSize w)) -- mayhap?

-- Compound
toUpRight pt w = toUp (toRight pt w) w

toUpLeft pt w = toUp (toLeft pt w) w

toDownRight pt w = toDown (toRight pt w) w

toDownLeft pt w = toDown (toLeft pt w) w

neighbours pt w = [toUp pt w, toDown pt w, toRight pt w, toLeft pt w, toUpRight pt w, toUpLeft pt w, toDownRight pt w, toDownLeft pt w]

-- get number of live neighbours
liveNeighbours listNeighbours w = length $ filter (live w) listNeighbours

deadNeighbours listNeighbours w = 8 - liveNeighbours listNeighbours w

live w pt = pt `elem` bSquares w

dead w pt = not $ live pt w

switchCell :: (Float, Float) -> World -> Bool
switchCell pt w
  | live w pt =
      liveNeighbours (neighbours pt w) w <= underpopulation w || liveNeighbours (neighbours pt w) w >= overpopulation w
  | dead pt w = liveNeighbours (neighbours pt w) w == reproduction w

step :: World -> World
step w = do
  let wSwitchers = filter (\pt -> switchCell pt w) (wSquares w)
  let bSwitchers = filter (\pt -> switchCell pt w) (bSquares w)
  killAll bSwitchers $ resurrectAll wSwitchers w

updateWorldIO :: Float -> World -> IO World
updateWorldIO int w =
  if state w == Started
    then return $ step w
    else return w
