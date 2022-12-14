import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Data.HashSet as HashSet

-- Day 6: Tuning Trouble
-- Part 1: Find the character pos at the end of the first 4 consecutive unique characters
-- Part 2: Find the character pos at the end of the first 14 consecutive unique characters

areCharsUnique :: String -> Bool
areCharsUnique str = areCharsUnique' str HashSet.empty
    where
        areCharsUnique' :: String -> HashSet Char -> Bool
        areCharsUnique' [] _ = True
        areCharsUnique' (s:str) set = not (HashSet.member s set) && (areCharsUnique' str (HashSet.insert s set))

sequenceMarkerChar :: String -> Int -> Int
sequenceMarkerChar input count = sequenceMarkerChar' input count
    where
        sequenceMarkerChar' str index
            |   areCharsUnique (take count str) = index
            |   otherwise = sequenceMarkerChar' (drop 1 str) (index  + 1)

partOne :: [String] -> Int
partOne lines = sequenceMarkerChar (head lines) 4

partTwo :: [String] -> Int
partTwo lines = sequenceMarkerChar (head lines) 14

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)