module AI where

import Board
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Time.Clock as Clock
import Debug.Trace
import System.IO.Unsafe

-- -- Update the world state after some time has passed
updateWorldIO :: Float -> World -> IO World
updateWorldIO int = return
