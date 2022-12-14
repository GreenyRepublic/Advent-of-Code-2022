import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Data.HashSet as HashSet

-- Day 6: Tuning Trouble
-- Part 1: Find the character at the end of the first 4 consecutive unique characters
-- Part 2: 

areCharsUnique :: String -> Bool
areCharsUnique str = areCharsUnique' str HashSet.empty
    where
        areCharsUnique' :: String -> HashSet -> Bool
        areCharsUnique' [] _ = True
        areCharsUnique' (s:str) set = not (HashSet.member s set) && (areCharsUnique' str (HashSet.insert s set))

sequenceMarkerChar :: String -> Int
sequenceMarkerChar input = sequenceMarkerChar' input 4
    where
        sequenceMarkerChar' str index
        | areCharsUnique (take 4 str) = index
        | otherwise = sequenceMarkerChar' (drop 1 str) (index  + 1)

partOne :: [String] -> Int
partOne lines = sequenceMarkerIndex (head lines)

partTwo :: [String] -> String
partTwo lines = ""

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)