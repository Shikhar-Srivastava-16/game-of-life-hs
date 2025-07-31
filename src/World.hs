module World where

import Data.List (delete)
data State = Stopped | Started
  deriving (Eq, Show)

data World = World
  { bound :: Float,
    state :: State,
    tSize :: Float,
    bSquares :: [(Float, Float)],
    wSquares :: [(Float, Float)],
    underpopulation :: Int,
    overpopulation :: Int,
    reproduction :: Int
  }
  deriving (Show, Eq)

-- Function to remove an element from a list
removeElement :: (Eq a) => a -> [a] -> [a]
removeElement = delete

-- Function to check if an element is in a list and move it between two lists
-- If the element is in list1, it's moved to list2
-- If the element is in list2 (and not list1), it's moved to list1
-- Otherwise, lists remain unchanged
moveElement :: (Eq a) => a -> [a] -> [a] -> ([a], [a])
moveElement element list1 list2
  | element `elem` list1 = (removeElement element list1, element : list2)
  | element `notElem` list1 && element `elem` list2 = (element : list1, removeElement element list2)
  | otherwise = (list1, list2)

kill cell w = w {bSquares = bs, wSquares = ws} where (bs, ws) = moveElement cell (bSquares w) (wSquares w)

killAll [] w = w
killAll [cell] w = kill cell w
killAll (cell:cells) w = killAll cells (kill cell w)

resurrect cell w = w {bSquares = bs, wSquares = ws} where (ws, bs) = moveElement cell (wSquares w) (bSquares w)

initWorld :: Float -> Float -> World
-- initWorld bDim bTarg filePath "" =
initWorld boardDims tileSize = do
  -- list of all intercepts in cartesian system along the
  let bs = [-a, (tileSize - a) .. a]
  let con = Prelude.head bs
  let loci = [[(con, b), (-con, b)] | b <- bs] ++ [[(b, con), (b, -con)] | b <- bs]

  World a Stopped 50 [] [(x, y) | x <- bs, y <- bs] 1 4 3
  where
    a = boardDims * 0.5 * tileSize
