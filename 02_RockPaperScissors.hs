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

charToOutcome :: String -> Outcome
charToOutcome "X" = Lose
charToOutcome "Y" = Draw
charToOutcome "Z" = Win
charToOutcome letter = Draw

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Lose = 0
outcomeScore Draw = 3

choiceScore :: PlayChoice -> Int
choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3

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

playForOutcome :: PlayChoice -> Outcome -> Int
playForOutcome opponent Lose = (outcomeScore Lose) + choiceScore (getLoser opponent)
playForOutcome opponent Draw = (outcomeScore Draw) + choiceScore (getDrawer opponent)
playForOutcome opponent Win = (outcomeScore Win) + choiceScore (getWinner opponent)

--

playRock :: PlayChoice -> Outcome
playRock Paper = Win
playRock Scissors = Lose
playRock Rock = Draw

playPaper :: PlayChoice -> Outcome
playPaper Scissors = Win
playPaper Rock = Lose
playPaper Paper = Draw

playScissors :: PlayChoice -> Outcome
playScissors Rock = Win
playScissors Paper = Lose
playScissors Scissors = Draw

chooseOption :: PlayChoice -> (PlayChoice -> Outcome)
chooseOption Rock = playRock
chooseOption Paper = playPaper
chooseOption Scissors = playScissors

playForOption :: PlayChoice -> PlayChoice -> Int
playForOption left right = outcomeScore ((chooseOption left) right) + (choiceScore right)

--

playGame :: [PlayChoice] -> Int
playGame input = playForOption left right
    where   left = head input
            right = last input

playGameRevised :: [String] -> Int
playGameRevised input = playForOutcome left right
    where   left = charToPlayChoice (head input)
            right = charToOutcome (last input)

partOne :: [String] -> Int
partOne values = sum (map playGame mappedValues)
    where   mappedValues = map (map charToPlayChoice) parsedValues
            parsedValues = parseInput values

partTwo :: [String] -> Int
partTwo values = sum (map playGameRevised parsedValues)
    where   parsedValues = parseInput values

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)