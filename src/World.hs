module World where

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).

data World = World
  { tSize :: Float,
    bSquares :: [(Float, Float)],
    wSquares :: [(Float, Float)]
  }
  deriving (Show, Eq)

initWorld :: Float -> Float -> World
-- initWorld bDim bTarg filePath "" =
initWorld boardDims tileSize = do
  -- bs is a list of points
  let bs = [-a, (tileSize - a) .. a] where a = boardDims * 0.5 * tileSize
  -- con is first thing in bs
  let con = Prelude.head bs
  -- loci are all the lines - specifically, a list of lists
  -- inner list is two points which are extreme ends of each line
  let loci = [[(con, b), (-con, b)] | b <- bs] ++ [[(b, con), (b, -con)] | b <- bs]

  World 50 [] [(x, y) | x <- bs, y <- bs]
