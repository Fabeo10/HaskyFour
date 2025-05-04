HaskyFour: Console-based Connect Four in Haskell

A pure Haskell implementation of the classic Connect Four game, built using the Model–View–Controller (MVC) pattern. Players take turns dropping discs into a 7×6 grid, aiming to connect four in a row horizontally, vertically, or diagonally (with wrap-around support).

🎯 Features

Pure Model (Board.hs)

Board creation and manipulation without any I/O.

Slot queries (numSlot, isSlotOpen, isFull).

Disc dropping logic (dropInSlot).

Quit the game anytime with q or Q

Win detection (isWonBy) for horizontal, vertical, and diagonal runs (with wrap-around).

Pretty-print conversion (boardToStr).

View/Controller (Main.hs)

Console-based UI for two-player gameplay.

Robust input handling (readSlot), with prompts and validation.

Turn-taking game loop (gameLoop) that prints the board, reads moves, and announces results.

Prerequisites

GHC (version 8.10 or higher recommended), or Hugs, etc.

▶️ Getting Started

Clone the repository:

git clone https://github.com/Fabeo10/HaskyFour.git
cd HaskyFour

Compile & Run with GHCi:

ghci Board.hs Main.hs
ghci> main

How to Play

On launch, you’ll see an empty 7×6 board printed as dots (.).

Players O and X take turns entering a column number (1–7) to drop their disc.

The board refreshes after each move.

The first player to connect four discs in a row (horizontally, vertically, or diagonally) wins.

If the board fills without any four-in-a-row, the game ends in a draw.

Project Structure

HaskyFour/
├── Board.hs      -- Model: board logic & win detection
├── Main.hs       -- View/Controller: I/O & game loop
├── README.md     -- This documentation
└── LICENSE       -- (optional) License file

Example Gameplay
Welcome to Connect Four!
Press 'q' at any time to exit.

Current board:
. . . . . . .
. . . . . . .
. . . . . . .
O . . . . . .
O X . . . . .
O X O X O X .

Player O, choose a column (1-7) or 'q' to quit:
> 


Coding Conventions & Guidelines

MVC separation: All I/O confined to Main.hs; Board.hs remains pure.

Prelude-only: No external libraries beyond standard Prelude and System.IO for flushing.

Error handling: Functions like dropInSlot and playerToChar use error on invalid inputs, while user-facing I/O loops validate and re-prompt.

Wrap-around logic: Win detection considers cyclical sequences at grid edges.

Extending the Game (In progress, will update if implemented...)

AI Opponent: Replace the otherPlayer swap in gameLoop with an AI move generator.

Custom dimensions: Parameterize cols and rows in main for different board sizes.

Scorekeeping & menus: Add a menu system with multiple rounds and score tracking.


❌ Exit Anytime

Type q or Q when prompted for a column to quit the game gracefully.

🛠 Built With

Haskell
Only standard prelude and modules (System.IO, System.Exit)


📁 License

This project is released under the MIT License. See LICENSE for details.