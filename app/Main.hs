-- Haskell Project -- Connect Four -- May 04, 2025
-------------------------------------------------------------------------------------------
-- Fabian Ornelas
-------------------------------------------------------------------------------------------
module Main where

    import Board
    import System.IO (hFlush, stdout) -- flush stdout after printing
    import System.Exit (exitSuccess)
    import System.Random (randomRIO) -- EC feature

    -- Game mode = PvP (Player vs Player) or PvC (Player vs Computer)
    data GameMode = PvP | PvC

 --- Part 1) ------------------------------------------------------------------------------
    -- Convert player number to display character discs: O for player 1 and X for player 2
    playerToChar :: Int -> Char
    playerToChar p
        | p == mkPlayer = 'O'
        | p == mkOpponent = 'X'
        | otherwise = error "playerToChar: unknown player"

    -- (1-based) Read a valid slot num from the user for player b on board bd
    -- Placed in a loop until a valid slot is entered
    readSlot :: Board -> Int -> IO Int
    readSlot bd p = do 
        putStrLn $ "Player " ++ [playerToChar p] ++ ", choose a column (1-" ++ show (numSlot bd) ++ ") or 'q' to quit: "
        putStr "> " 
        hFlush stdout -- flush stdout to ensure prompt appears immediately
        line <- getLine
        case line of
            "q" -> putStrLn "Exiting the game..." >> exitSuccess
            "Q" -> putStrLn "Exiting the game..." >> exitSuccess
            _ -> case reads line :: [(Int, String)] of
                [(i, _)] | isSlotOpen bd i -> return i
                        | otherwise -> invalid "The slot is full or out of range. "
                _ -> invalid "Invalid input. Please enter a number. "
        where
            invalid msg = do
                putStrLn msg
                readSlot bd p

 --- Extra Credit (EC) feature) ----------------------------------------------------------
    -- Randomly select a slot for the opponent
    getRandomSlot :: Board -> IO Int
    getRandomSlot bd = do
        let openCols = [i | i <- [1..numSlot bd], isSlotOpen bd i]
        -- If called correctly, this move guarantees that move is valid
        idx <- randomRIO (0, length openCols - 1)
        return (openCols !! idx)

    -- Prompt user to select game mode
    chooseGameMode :: IO GameMode
    chooseGameMode = do
        putStrLn "Select game mode:"
        putStrLn "1. Human vs Human"
        putStrLn "2. Human vs Computer"
        putStr "Enter 1 or 2: "
        hFlush stdout
        input <- getLine
        case input of
            "1" -> return PvP
            "2" -> return PvC
            _   -> do
                putStrLn "Invalid input. Try again."
                chooseGameMode
                
 --- Part 2) ------------------------------------------------------------------------------
    -- Implement the main game loop, to display board, read move, apply, and recurse until win or draw
    gameLoop :: GameMode -> Board -> Int -> IO ()
    gameLoop mode bd p = do
        putStrLn "\nCurrent board:"   -- print a blank line then header
        putStrLn $ boardToStr playerToChar bd
        if isWonBy bd (otherPlayer p)
            then
                putStrLn $ "Player " ++ [playerToChar (otherPlayer p)] ++ " wins!"
        else if isFull bd
            then
                putStrLn "It's a draw!"
        else do
            -- If it is the computer's turn, use getRandomSlot; otherwise user input
            i <- case mode of
                PvP -> readSlot bd p
                PvC -> 
                    if p == mkOpponent
                        then do
                            putStrLn "Computer is thinking..."
                            getRandomSlot bd
                        else 
                            readSlot bd p
            let bd' = dropInSlot bd i p
            gameLoop mode bd' (otherPlayer p)

    -- Swap player 1 <-> opponent 
    otherPlayer :: Int -> Int
    otherPlayer p
        | p == mkPlayer = mkOpponent
        | p == mkOpponent = mkPlayer
        | otherwise = error "otherPlayer: unknown player"

    -- Entry point of the program, Init and start!
    main :: IO ()
    main = do
        let cols = 7
            rows = 6
            board = mkBoard cols rows
        putStrLn "Welcome to Connect Four!"
        mode <- chooseGameMode
        putStrLn "Player 1 is 'O' and Player 2 is 'X'."
        putStrLn "Press 'q' any time to exit."
        gameLoop mode board mkPlayer