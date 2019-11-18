import System.IO

main = play [5,4,3,2,1]

play :: [Int] -> IO ()
play board = do
    (newBoard, winner) <- takeTurns board
    case winner of
        0 -> play newBoard
        1 -> do putStrLn "You won!"
                return ()
        2 -> do putStrLn "Computer won!"
                return ()

takeTurns :: [Int] -> IO ([Int],Int)
takeTurns board = do
    newBoard <- playerTurn board
    if sum newBoard == 0 then
        return (newBoard, 1)
    else do
        newBoard <- computerTurn newBoard    
        case sum newBoard of
            0 -> return (newBoard, 2)
            _ -> return (newBoard, 0)

playerTurn :: [Int] -> IO [Int]
playerTurn board = do
    putStrLn "\nYour turn"
    displayBoard board
    row   <- getRow board
    count <- getInt "How many stars? " 1 (board!!row)
    return $ (take row board) ++ [board!!row - count] ++ drop (row+1) board

computerTurn :: [Int] -> IO [Int]
computerTurn board =
    let (row, count) = makeTurn board 0 in
    return $ take row board ++ [board!!row - count] ++ drop (row+1) board

makeTurn :: [Int] -> Int -> (Int, Int)
makeTurn board row_num =
    let how_many = board!!row_num in
    if how_many == 0 then
        makeTurn board (row_num +1)
    else
        (row_num, 1)



displayBoard :: [Int] -> IO ()
displayBoard [] = return ()
displayBoard board = do
    putStrLn $ show (length board) ++ " : " ++ replicate (last board) '*'
    displayBoard (init board)

getRow :: [Int] -> IO Int
getRow board = do
    row <- getInt "Which row? " 1 (length board)
    if board!!(row-1) == 0
      then do putStrLn "That row is empty!"
              getRow board
      else return (row-1)

getInt msg min max = do
    putStrLn msg
    input <- getLine
    let parsed = reads input :: [(Int,String)] --zamiana stringa na int i resztę stringa
    if null parsed
      then badNumber "Not a number"
      else testNumber (fst (head parsed))
    where
        badNumber error = do putStrLn error
                             getInt msg min max
        testNumber number
            | number < min = badNumber "Too small"
            | number > max = badNumber "Too big"
            | otherwise    = return number
