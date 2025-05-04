🎮 HaskyFour: Console-Based Connect Four in Haskell

A functional Haskell implementation of the classic Connect Four game, following the Model–View–Controller (MVC) pattern. Drop discs, connect four, and optionally play against a simple AI!

✨ Features

✅ Pure Model (Board.hs)

Pure functional logic with no I/O.

Functions for board creation, slot queries, and disc drops.

Full win detection:

Horizontal, vertical, and diagonal (with wrap-around logic).

Graceful handling of full boards and illegal moves.

Board rendering via boardToStr.

🧠 View/Controller (Main.hs)

Interactive console UI with turn-based gameplay.

Robust input validation: column numbers or quitting with q.

AI opponent support (optional):

Uses System.Random to generate random valid moves.

Turn-taking game loop prints board and announces results.

🧱 Project Structure

HaskyFour/

├── app/

│   ├── Main.hs       -- Controller & UI

│   └── Board.hs      -- Pure board logic

├── HaskyFour.cabal   -- Cabal project file

└── README.md         -- This file

🧑‍💻 Prerequisites

GHC (v8.10 or higher recommended)
Cabal (for dependency management and running)
Internet connection (for installing the random package)

🚀 Getting Started

Clone the repo:

git clone https://github.com/Fabeo10/HaskyFour.git

cd HaskyFour

Initialize the project:

cabal update

cabal build

Run interactively:

cabal repl

Launches the game in GHCi:

ghci> main

🕹️ How to Play

At launch, you'll be asked to select a game mode.
Then you'll see a blank 7×6 board.
Players O and X take turns entering a column number (1–7).
Type q at any time to quit.
First to connect four in a row wins!
If the board fills without a winner, it's a draw.

🧠 AI Mode (Bonus Feature)

You can replace one of the human players with a computer opponent:

Uses randomRIO to choose a valid slot.
Implemented via the getRandomSlot function.
Make sure random is listed in your .cabal file under build-depends:

  build-depends:
      base >=4.7 && <5,
      random
🔧 Customization Ideas

✅ Custom board sizes

✅ Smarter AI strategies

🔜 Scorekeeping across rounds

🔜 Menus for game mode selection

✅ Coding Guidelines

MVC separation: Board.hs has no I/O, only pure logic.
Uses only Prelude, System.IO, System.Exit, and System.Random.
Proper error handling for both logic and user input.

❌ Exit Anytime

Press q or Q when prompted to gracefully exit the game.

🛠 Built With

Haskell
GHC & Cabal

📄 License

MIT License – see the LICENSE file for details.