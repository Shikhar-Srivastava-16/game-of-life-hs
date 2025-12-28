module Main where

import Draw
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game
import Input
import Options.Applicative
import Update
import World

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

-- parser library for CLI flags: https://hackage.haskell.org/package/optparse-applicative
data CLIArgs = CLIArgs
  { argSize :: Float,
    argSpd :: Int,
    argTile :: Float
  }

cliParser :: Parser CLIArgs
cliParser =
  CLIArgs
    -- parser for size
    <$> option
      auto
      ( long "bsize"
          <> short 's'
          <> metavar "<SIZE>"
          <> value 10
          <> help "The size of the board"
      )
    <*> option
      auto
      ( long "speed"
          <> short 'v'
          <> metavar "<FPS>"
          <> value 10
          <> help "The speed at which game loop runs, i.e the number of times the loop functions are called per second"
      )
    <*> option
      auto
      ( long "tile"
          <> short 't'
          <> metavar "<TILESIZE>"
          <> value 20
          <> help "The size of a single tile/cell"
      )

main :: IO ()
main = do
  args <- execParser cliargs
  let tSize = argTile args
  let dims = 2 * argSize args
  (xS, yS) <- getScreenSize
  let winDim = round (dims * tSize + tSize + 10)
  playIO
    (InWindow "Conway's Game of Life" (winDim, winDim) ((xS - winDim) `div` 2, (yS - winDim) `div` 2))
    (makeColor 0.23 0.9 1 1)
    (argSpd args)
    (initWorld dims tSize) -- in Board.hs
    drawIOWorld -- in Draw.hs
    handleInputIO -- in Input.hs
    updateWorldIO -- in AI.hs
  where
    cliargs =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> header "Starting up Conway's Game of Life"
            <> progDesc "Conway's Game of Life, written in haskell!"
        )

-- Wrappers to run these functions 'inside' the IO monad, so that we can read from and write to files
drawIOWorld :: World -> IO Picture
drawIOWorld w = return $ drawWorld w
