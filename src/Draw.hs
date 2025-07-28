module Draw (drawWorld) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Time.Clock as Clock
import Graphics.Gloss
import System.IO.Unsafe
import World

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld = drawGrid

--  Pictures (drawGrid (tSize w) 10 [] [])

square :: Color -> Float -> (Float, Float) -> Picture
square colour size (x, y) = Color colour $ Polygon [(x + size / 2, y + size / 2), (x - size / 2, y + size / 2), (x - size / 2, y - size / 2), (x + size / 2, y - size / 2)]

-- Old drawGrid function for drawing purely rather than with bitmap images

drawGrid w = Pictures $ [square white (tSize w - 5) pt | pt <- wSquares w] ++ [square black (tSize w - 5) pt | pt <- bSquares w]

{--

drawGrid tSize bDims wSquares bSquares = do
  -- bs is a list of points
  let bs = [-a, (tSize - a) .. a] where a = bDims * 0.5 * tSize
  -- con is first thing in bs
  let con = head bs
  -- loci are all the lines - specifically, a list of lists
  -- inner list is two points which are extreme ends of each line
  let loci = [[(con, b), (-con, b)] | b <- bs] ++ [[(b, con), (b, -con)] | b <- bs]
  let wPics = [translate xi yi (Color white $ circleSolid $ tSize * 0.4) | (xi, yi) <- wSquares]
  let bPics = [translate xi yi (Color black $ circleSolid $ tSize * 0.4) | (xi, yi) <- bSquares]
  [Color white $ Line locus | locus <- loci] ++ wPics ++ bPics
--}
