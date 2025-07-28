module Update where

import Debug.Trace
import World

-- Update the world state after some time has passed
updateWorldIO :: Float -> World -> IO World
updateWorldIO int = return
