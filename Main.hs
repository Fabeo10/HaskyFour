-- Haskell Project -- Connect Four -- May 04, 2025
-------------------------------------------------------------------------------------------
-- Fabian Ornelas
-------------------------------------------------------------------------------------------
module Main where

    import Board
    import System.IO (hFlush, stdout) -- flush stdout after printing

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
        putStrLn $ "Player " ++ [playerToChar p] ++ ", choose a column (1-" ++ show (numSlot bd) ++ "): "
        putStr "> " 
        hFlush stdout -- flush stdout to ensure prompt appears immediately
        line <- getLine
        case reads line :: [(Int, String)] of
            [(i, _)] | isSlotOpen bd i -> return i
                     | otherwise -> invalid "The slot is full or out of range. "
            _ -> invalid "Invalid input. Please enter a number. "
        where
            invalid msg = do
                putStrLn msg
                readSlot bd p
                
 --- Part 2) ------------------------------------------------------------------------------
    -- Implement the main game loop, to display board, read move, apply, and recurse until win or draw
    gameLoop :: Board -> Int -> IO ()
    gameLoop bd p = do
        putStrLn "\nCurrent board:"   -- print a blank line then header
        putStrLn $ boardToStr playerToChar bd
        if isWonBy bd (otherPlayer p)
            then
                putStrLn $ "Player " ++ [playerToChar (otherPlayer p)] ++ " wins!"
            else if isFull bd
                then
                    putStrLn "It's a draw!"
                else do
                    i <- readSlot bd p
                    let bd' = dropInSlot bd i p
                    gameLoop bd' (otherPlayer p)

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
        gameLoop board mkPlayer