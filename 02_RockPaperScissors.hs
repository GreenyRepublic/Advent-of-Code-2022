import System.IO
import System.Environment
import Data.List.Split
import Data.List

-- Day 2: Rock Paper Scissors
-- Part 1: Find the total score for the given strategy guide
-- Part 2: Find total score with revised strategy guide

data Outcome = Win | Lose | Draw
data PlayChoice = Rock | Paper | Scissors

charToPlayChoice :: String -> PlayChoice
charToPlayChoice input
    | input == "A" || input == "X" = Rock
    | input == "B" || input == "Y" = Paper
    | otherwise = Scissors

outcomeToScore :: Outcome -> Int
outcomeToScore Win = 6
outcomeToScore Lose = 0
outcomeToScore Draw = 3

choiceToScore :: PlayChoice -> Int
choiceToScore Rock = 1
choiceToScore Paper = 2
choiceToScore Scissors = 3

parseInput :: [String] -> [[String]]
parseInput input = map (splitOn " ") input

--

getWinner :: PlayChoice -> PlayChoice
getWinner Rock = Paper
getWinner Paper = Scissors 
getWinner Scissors = Rock

getDrawer :: PlayChoice -> PlayChoice
getDrawer choice = choice

getLoser :: PlayChoice -> PlayChoice
getLoser Rock = Scissors
getLoser Paper = Rock
getLoser Scissors = Rock

playForOutcome :: String -> String -> Int
playForOutcome opponent Rock = 0 + (choiceToScore (getLoser opponent))
playForOutcome opponent Paper = 3 + (choiceToScore (getDrawer opponent))
playForOutcome opponent Scissors = 6 + (choiceToScore (getWinner opponent))
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
playGame input = (winLossValue (left)) (right) + (choiceToScore right)
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