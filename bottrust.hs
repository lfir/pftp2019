import Control.Monad
import Data.Char(digitToInt, isAlphaNum, isDigit)

-- Takes a String formatted like the input files and returns a Tuple List with the robot's color and the buttons to be pressed.
strToTuple :: String -> [(Char, Int)]
strToTuple [] = []
strToTuple (c1:cs) = (c1, read(takeWhile isDigit cs)) : (strToTuple (dropWhile isDigit cs))

-- Returns the List of button sequences (Tuple List) from a List of String (one for each line of the input file).
strsToTuple :: [String] -> [[(Char, Int)]]
strsToTuple ss = map strToTuple (map (filter isAlphaNum) ss)

-- Takes one button sequence and returns the Int representing the minimum number of seconds required to push them.
solution :: [(Char,Int)] -> Int
solution xs = countSeconds 0 (buttonO xs) (buttonB xs) 1 1 xs

-- Takes a button sequence and returns the position where the next button for the 'O' robot is. If there is no button returns 0.
buttonO :: [(Char,Int)] -> Int
buttonO [] = 0
buttonO (('O',i):io) = i
buttonO (x:xs) = buttonO xs

-- Takes a button sequence and returns the position where the next button for the 'B' robot is. If there is no button returns 0.
buttonB :: [(Char,Int)] -> Int
buttonB [] = 0
buttonB (('B',i):ib) = i
buttonB (x:xs) = buttonB xs

-- Returns the minimum number of seconds to complete the button sequence.
-- t = seconds total. o = O's next button position. b = B's next button position. 
-- po = O's current hallway position. pb = B's current position.
countSeconds :: Int -> Int -> Int -> Int -> Int -> [(Char,Int)]-> Int
countSeconds t _ _ _ _ [] = t
countSeconds t o b po pb (('O',o'):xs) = if (po == o') 
										   then 1 + countSeconds t (buttonO xs) b po (updateHallway pb b) xs
										 else 1 + countSeconds t o b (updateHallway po o) (updateHallway pb b) (('O',o'):xs)
countSeconds t o b po pb (('B',b'):xs) = if (pb == b') 
										   then 1 + countSeconds t o (buttonB xs) (updateHallway po o) pb xs
										 else 1 + countSeconds t o b (updateHallway po o) (updateHallway pb b) (('B',b'):xs)

-- Returns the hallway position after moving one step in the direction of the next target.
updateHallway:: Int -> Int -> Int
updateHallway n m = if (n == m) then n else if (n < m) then (n + 1) else (n - 1)

-- Returns a List with a String for each line in the inputs file.
getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

-- Returns a String formatted for output.
addPrefix:: Int -> String -> String
addPrefix n s = ("Case #" ++ (show n) ++ ": " ++ s)

-- Returns a formatted String List (with case number added to each one).
addPrefixes :: Int -> [String] -> [String]
addPrefixes n xs = foldr (\x r n' -> (addPrefix n' x) : (r (n'+1))) (\n' -> []) xs n

main = do
    putStrLn "Enter the path to the inputs file."
    path <- getLine
    list <- getLines path
    let procList = strsToTuple (map (tail . dropWhile isDigit) (tail list))
    let res = map (solution) procList
	-- Write formatted solution lines to file.
    writeFile "res.txt" $ unlines (addPrefixes 1 (map show res))

