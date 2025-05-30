-- Haskell Project -- Connect Four -- May 04, 2025
-------------------------------------------------------------------------------------------
-- Fabian Ornelas
-------------------------------------------------------------------------------------------
module Board (Board, mkBoard, mkPlayer, mkOpponent, numSlot, isSlotOpen, isFull, dropInSlot, isWonBy, boardToStr) where
 --- Part 1) ------------------------------------------------------------------------------
    -- Let's create a board for the game and access its elements
    type Board = [[Int]]

    -- We create a board size m x n, where m is the no. of columns and n is the no. of rows
    -- Initialize the board with 0s, representing empty slots
    mkBoard :: Int -> Int -> Board
    mkBoard m n
        | m > 0 && n > 0 = replicate n (replicate m 0)
        | otherwise = error "mkBoard: Invalid dimensions"

    -- Here we create player 1 and player 2, represented by 1 and 2, respectively
    mkPlayer :: Int
    mkPlayer = 1

    mkOpponent :: Int
    mkOpponent = 2

 ------- Part 2) --------------------------------------------------------------------------
    -- We start by creating a function to return the number of columns in the board
    numSlot :: Board -> Int
    numSlot [] = 0
    numSlot (row : _) = length row

    -- Check if the slot is open here by checking if column i has an empty cell. (1-based)
    isSlotOpen :: Board -> Int -> Bool
    isSlotOpen bd i
        | i < 1 || i > numSlot bd = False
        | otherwise = any (\row -> row !! (i - 1) == 0) bd

    -- This functions checks if the board is full by checking if all columns are filled
    isFull :: Board -> Bool
    isFull bd = all (/= 0) (concat bd)

    -- Drop player p's disc in the board, assuming column empty is valid
    dropInSlot :: Board -> Int -> Int -> Board
    dropInSlot bd i p 
        | not (isSlotOpen bd i) = error "dropInSlot: Slot not open"
        | otherwise = reverse (go (reverse bd))
        where
            idx = i - 1
            go [] = error "dropInSlot: No empty slots found"
            go (row:rows)
                | row !! idx == 0 = replaceAt idx p row : rows
                | otherwise = row : go rows

    -- Helper function: Replaces the element at index n (0-based) with x. Where x is the player
    replaceAt :: Int -> a -> [a] -> [a]
    replaceAt n x xs = take n xs ++ [x] ++ drop (n + 1) xs

 --- Part 3) ------------------------------------------------------------------------------
    -- Check if the player has won by checking if there are 4 in a row, includes wrap around
    -- Later, implemented diagonal checks for a complete game
    isWonBy :: Board -> Int -> Bool
    isWonBy bd p = any (hasRun p k) (rows ++ cols) || any (hasDiag p k) directions
        where
            k = 4
            rows = bd
            cols = transposeBoard bd
            -- Check a linear run in a list (wrap-around)
            hasRun player len xs = any isWin [0 .. length xs - 1]
                where
                    ext = xs ++ take (len - 1) xs
                    isWin i = all (== player) (take len (drop i ext))
            -- Diagonal checks: directions are (dRow, dCol)
            directions = [(1,1), (-1,1)]
            numRows = length bd
            numCols = numSlot bd
            -- Check diagonals for a direction
            hasDiag player len (dR, dC) = or
                [ all (\i -> getCell bd ((r + i * dR) `mod` numRows) ((c + i * dC) `mod` numCols) == player) [0..len-1]
                | r <- [0..numRows-1], c <- [0..numCols-1]
                ]

    -- Helper function to safely retrieve a cell from the board with wrap-around
    getCell :: Board -> Int -> Int -> Int
    getCell bd r c = (bd !! r) !! c

    -- Tranpose the board implemented with pattern matching
    transposeBoard :: Board -> Board
    transposeBoard xss
        | any null xss = []
        | otherwise    = [ h | (h:_) <- xss ] : transposeBoard [ t | (_:t) <- xss ]

 --- Part 4) ------------------------------------------------------------------------------
    -- Convert the board to a string representation
    boardToStr :: (Int -> Char) -> Board -> String
    boardToStr playerToChar bd = unlines $ map renderRow bd
        where
            renderRow :: [Int] -> String
            renderRow = unwords . map toStr 
            toStr :: Int -> String
            toStr 0 = "."
            toStr p = [playerToChar p]