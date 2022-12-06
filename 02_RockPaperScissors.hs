import System.IO
import System.Environment
import Data.List.Split
import Data.List

-- Day 2: Rock Paper Scissors
-- Part 1: Find the total score for the given strategy guide
-- Part 2: Find total score with revised strategy guide

data Outcome = Win | Lose | Draw deriving (Eq, Show)
data PlayChoice = Rock | Paper | Scissors deriving (Eq, Show)

rpsCycle = [Rock, Paper, Scissors]
wldCycle = [Draw, Win, Lose]

rotateLeft :: [a] -> Int -> [a]
rotateLeft vals 0 = vals
rotateLeft (v:vals) n = rotateLeft (vals ++ [v]) (n - 1)

rotateRight :: [a] -> Int -> [a]
rotateRight vals 0 = vals
rotateRight vals n = rotateRight ((last vals):vals) (n - 1)

charToPlayChoice :: Char -> PlayChoice
charToPlayChoice input
    | input == 'A' || input == 'X' = Rock
    | input == 'B' || input == 'Y' = Paper
    | otherwise = Scissors

charToOutcome :: Char -> Outcome
charToOutcome input
    | input == 'X' = Lose
    | input == 'Y' = Draw
    | input == 'Z' = Win

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Lose = 0
outcomeScore Draw = 3

choiceScore :: PlayChoice -> Int
choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3

choiceIndex :: PlayChoice -> Int
choiceIndex choice = (choiceScore choice) - 1

parseInput :: [String] -> [[String]]
parseInput input = map (splitOn " ") input

parseInputToChoiceOutcome :: [String] -> [(PlayChoice, Outcome)]
parseInputToChoiceOutcome [] = []
parseInputToChoiceOutcome (v:values) = (charToPlayChoice (head v), charToOutcome (last v)) : parseInputToChoiceOutcome values

parseInputToChoices :: [String] -> [(PlayChoice, PlayChoice)]
parseInputToChoices [] = []
parseInputToChoices (v:values) = (charToPlayChoice (head v), charToPlayChoice (last v)) : parseInputToChoices values

--

getWinner :: PlayChoice -> PlayChoice
getWinner opt = getWinner' rpsCycle opt
    where
        getWinner' (v:values) choice
            | v == choice = head values
            | otherwise = getWinner' (rotateLeft (v:values) 1) choice

getLoser :: PlayChoice -> PlayChoice
getLoser opt = getLoser' rpsCycle opt
    where
        getLoser' (v:values) choice
            | v == choice = last values
            | otherwise = getLoser' (rotateLeft (v:values) 1) choice

playForOutcome :: PlayChoice -> Outcome -> Int
playForOutcome opponent Lose = (outcomeScore Lose) + choiceScore (getLoser opponent)
playForOutcome opponent Draw = (outcomeScore Draw) + choiceScore opponent
playForOutcome opponent Win = (outcomeScore Win) + choiceScore (getWinner opponent)

--

playRPS :: PlayChoice -> PlayChoice -> Outcome
playRPS left right = head (rotateLeft wldCycle indexDiff)
    where   indexDiff = mod (3 + ((choiceIndex right) - (choiceIndex left))) 3

playForOption :: PlayChoice -> PlayChoice -> Int
playForOption left right = outcomeScore (playRPS left right) + (choiceScore right)

--

playGame :: (PlayChoice,PlayChoice) -> Int
playGame input = playForOption left right
    where   left = fst input
            right = snd input

playGameRevised :: (PlayChoice, Outcome) -> Int
playGameRevised input = playForOutcome left right
    where   left = fst input
            right = snd input

partOne :: [String] -> Int
partOne values = sum (map playGame parsedValues)
    where   parsedValues = parseInputToChoices values

partTwo :: [String] -> Int
partTwo values = sum (map playGameRevised parsedValues)
    where   parsedValues = parseInputToChoiceOutcome values

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)