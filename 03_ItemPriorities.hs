import System.IO
import System.Environment
import Data.List.Split
import Data.List

-- Day 3: Item Priorities
-- Part 1: Find sum of priorities of items that appear in both parts of each bags
-- Part 2: Find sum of priorities of items that appear in each triple of bags

charPriority :: Char -> Int
charPriority input = charPriority' input list
    where 
        charPriority' char charList
            | charList == [] = 0
            | fst (head charList) == char = snd (head charList)
            | otherwise = charPriority' char (tail charList)
        list = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

halfList :: [a] -> ([a], [a])
halfList items = (take half items, drop half items)
    where half = div (length items) 2

findDuplicateChar :: String -> String -> Char
findDuplicateChar [] right = '_'
findDuplicateChar left right
    | elem (head left) (right) = head left
    | otherwise = findDuplicateChar (tail left) right


partOne :: [String] -> Int
partOne values = sum (map charPriority (map (uncurry findDuplicateChar) halvedList))
    where
        halvedList = map halfList values

partTwo :: [String] -> Int
partTwo values = 0

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)