{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Board where

import Control.Applicative
import Control.Monad
import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Key
import qualified Data.ByteString.Lazy as B
import Data.Text
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Time.Clock as Clock
import Debug.Trace
import GHC.Generics
import Graphics.Gloss
import System.IO.Unsafe

data Col = Black | White
  deriving (Show, Generic, Eq)

instance FromJSON Col

instance ToJSON Col

other :: Col -> Col
other Black = White
other White = Black

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
  deriving (Show, Generic)

instance FromJSON World

initWorld :: World
-- initWorld bDim bTarg filePath "" =
initWorld = World 50 [] []
