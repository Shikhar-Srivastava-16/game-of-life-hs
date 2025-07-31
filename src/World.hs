module World where

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
