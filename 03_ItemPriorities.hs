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

findDuplicateChar :: [String] -> Char
findDuplicateChar ([]:items) = '_'
findDuplicateChar (a:items)
    | (foldr (&&) True (map (elem (head a)) items)) = head a
    | otherwise = findDuplicateChar ((tail a):items)


partOne :: [String] -> Int
partOne values = sum (map charPriority (map findDuplicateChar halvedList))
    where
        halvedList = map toList (map halfList values)
        toList (left, right) = [left, right]

partTwo :: [String] -> Int
partTwo [] = 0
partTwo (a:[]) = 0
partTwo (a:b:[]) = 0
partTwo (a:b:c:values) = charPriority (findDuplicateChar [a,b,c]) + (partTwo values)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)