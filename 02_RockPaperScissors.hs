import System.IO
import System.Environment
import Data.List.Split
import Data.List

-- Day 2: Rock Paper Scissors
-- Part 1: Find the total score for the given strategy guide
-- Part 2: Find total score with revised strategy guide

parseInput :: [String] -> [[String]]
parseInput input = map (splitOn " ") input

choiceValue :: String -> Int
choiceValue "X" = 1
choiceValue "Y" = 2
choiceValue "Z" = 3

getWinner :: String -> String
getWinner "A" = "Y"
getWinner "B" = "Z"
getWinner "C" = "X"

getDrawer :: String -> String
getDrawer "A" = "X"
getDrawer "B" = "Y"
getDrawer "C" = "Z"

getLoser :: String -> String
getLoser "A" = "Z"
getLoser "B" = "X"
getLoser "C" = "Y"

playForOutcome :: String -> String -> Int
playForOutcome opponent "X" = 0 + (choiceValue (getLoser opponent))
playForOutcome opponent "Y" = 3 + (choiceValue (getDrawer opponent))
playForOutcome opponent "Z" = 6 + (choiceValue (getWinner opponent))
playForOutcome a b = 0

--

playRock :: String -> Int
playRock "X" = 3
playRock "Y" = 6
playRock "Z" = 0

playPaper :: String -> Int
playPaper "X" = 0
playPaper "Y" = 3
playPaper "Z" = 6

playScissors :: String -> Int
playScissors "X" = 6
playScissors "Y" = 0
playScissors "Z" = 3

winLossValue :: String -> (String -> Int)
winLossValue "A" = playRock
winLossValue "B" = playPaper
winLossValue "C" = playScissors

playGame :: [String] -> Int
playGame input = (winLossValue (left)) (right) + (choiceValue right)
    where   left = head input
            right = last input

playGameRevised :: [String] -> Int
playGameRevised input = (playForOutcome left right)
    where   left = head input
            right = last input

partTwo :: [String] -> Int
partTwo values = sum (map (playGameRevised) parsedValues)
    where parsedValues = parseInput values

partOne :: [String] -> Int
partOne values = sum (map (playGame) parsedValues)
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