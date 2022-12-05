import System.IO
import System.Environment
import Data.List.Split
import Data.List

-- Day 1: Calorie Counting
-- Part 1: Find the most calories carried by a single elf
-- Part 2: Find the total calories carried by the top three elves

parseIntLists :: [String] -> [[Int]]
parseIntLists values = map (map (read::String->  Int)) (splitOn [""] values)

partTwo :: [String] -> Int
partTwo values = sum (take 3 (reverse (sort (map (sum) (parseIntLists values)))))

partOne :: [String] -> Int
partOne values = maximum (map (sum) (parseIntLists values))

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)