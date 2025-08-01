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

square :: Color -> Float -> (Float, Float) -> Picture
square colour size (x, y) = Color colour $ Polygon [(x + size / 2, y + size / 2), (x - size / 2, y + size / 2), (x - size / 2, y - size / 2), (x + size / 2, y - size / 2)]

drawGrid w = Pictures $ [square white (tSize w * 0.9) pt | pt <- wSquares w] ++ [square black (tSize w * 0.9) pt | pt <- bSquares w]
