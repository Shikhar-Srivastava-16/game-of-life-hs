# game-of-life-hs
Conway-s Game of Life, made in haskell!

# Controls:

## In-Game Controls

- Keystroke 's': performs one life-cycle of the Game of Life when it is paused
- Keystroke 'p': toggles Play/Pause. When Playing, the game will continuously perform life cycles until paused
- Left-Click anywhere on a cell using the mouse will toggle that cell's state between Live and Dead

## CLI Arguments

Use the -h flag when running to get help with the CLI Arguments. 
- -s,--bsize <SIZE>        The size of the board, i.e number of cells one each side of the board
- -v,--speed <FPS>         The speed at which game loop runs, i.e the number of 'life cycles' per second
- -t,--tile <TILESIZE>     The size of a single tile, which is the visual representation of one cell
