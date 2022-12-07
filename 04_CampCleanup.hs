import System.IO
import System.Environment
import Data.List.Split
import Data.List

-- Day 4: Camp Cleanup
-- Part 1: Find interval pairs where one is contained within another
-- Part 2: Find interval pairs where they overlap

data Interval = Interval { start :: Int, end :: Int } deriving (Eq, Show)

parseInterval :: String -> Interval
parseInterval input = Interval {start = read (head splitInput)::Int, end  = read (last splitInput)::Int}
    where splitInput = splitOn "-" input

parseIntervalPair :: String -> (Interval, Interval)
parseIntervalPair input = (parseInterval left, parseInterval right)
    where   left = head split
            right = last split
            split = (splitOn "," input)

parseInput :: [String] -> [(Interval, Interval)]
parseInput input = map parseIntervalPair input

doIntervalsEnclose :: Interval -> Interval -> Bool
doIntervalsEnclose (Interval sa ea) (Interval sb eb)  = ((sa >= sb) && (ea <= eb)) || ((sb >= sa) && (eb <= ea))

doIntervalsOverlap :: Interval -> Interval -> Bool
doIntervalsOverlap a b = not ((start a < start b && end a < start b) || (start a > end b && end a > end b))

partOne :: [String] -> Int
partOne values = length (filter (== True) (map (uncurry doIntervalsEnclose) parsedValues))
    where parsedValues = parseInput values

partTwo :: [String] -> Int
partTwo values = length (filter (== True) (map (uncurry doIntervalsOverlap) parsedValues))
    where parsedValues = parseInput values

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)