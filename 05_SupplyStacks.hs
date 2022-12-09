import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Data.Char

-- Day 5: Supply Stacks
-- Part 1: After rearrangement, which crate is at the top of each stack.
-- Part 2: 

data Instruction = Instruction {from :: Int, to :: Int, count :: Int} deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction str = Instruction {from = digitToInt (last (init str)), to = digitToInt (last str), count = read (take ((length str) - 2) str)::Int}

parseInstructionList :: [String] -> [Instruction]
parseInstructionList input = map parseInstruction (map (filter (\x -> elem x "0123456789")) input)

--

parseCrate :: String -> Char
parseCrate "   " = ' '  
parseCrate ('[' : c : "]") = c
parseCrate str = '_'

parseCrateRow :: String -> String
parseCrateRow [] = []
parseCrateRow input = (parseCrate (take 3 input)) : (parseCrateRow (drop 4 input))

parseCrateStacks :: [String] -> [String]
parseCrateStacks input = map (filter (\x -> x /= ' ')) (transpose (map parseCrateRow input))

--

parseInput :: [String] -> ([String], [Instruction])
parseInput input = (parseCrateStacks (init (head splitInput)), parseInstructionList(last splitInput))
    where splitInput = splitOn [""] input

runSingleInstruction :: [String] -> Instruction -> [String]
runSingleInstruction stack (Instruction from to count)
    | from <= to = runSingleInstruction' stack (Instruction (from - 1) (to - 1) count) []
    | otherwise = reverse(runSingleInstruction' (reverse stack) (Instruction ((length stack) - from) ((length stack) - to) count) []) 
    where
        runSingleInstruction' :: [String] -> Instruction -> String -> [String] 
        runSingleInstruction' [] instruction pick = []
        runSingleInstruction' (s:stack) (Instruction 0 0 count) pick = ((reverse pick) ++ s) : stack
        runSingleInstruction' (s:stack) (Instruction 0 to count) pick = (drop count s) : (runSingleInstruction' stack (Instruction 0 (to - 1) count) (take count s))
        runSingleInstruction' (s:stack) (Instruction from to count) pick = s : (runSingleInstruction' stack (Instruction (from - 1) (to - 1) count) pick)

runAllInstructions :: [String] -> [Instruction] -> [String]
runAllInstructions stacks inst = (runAllInstructions (runSingleInstruction stacks (head inst)) (tail inst))

partOne :: [String] -> String
partOne lines = map head (runAllInstructions (fst parsedLines) (snd parsedLines))
    where parsedLines = parseInput lines

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