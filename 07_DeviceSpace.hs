import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Data.Map as Map

-- Day 7: No Space Left On Device
-- Part 1: Find sum of total sizes of directories
-- Part 2: 

data FileSys = File {fileName::String, fileSize::Int} | Directory {dirName::String, dirContents::(Map String FileSys)} deriving (Show, Eq)

parseFileSystemLine :: String -> FileSys
parseFileSystemLine input
    | head splitInput == "dir" = Directory (last splitInput) Map.empty
    | otherwise = File (last splitInput) (read (head splitInput)::Int)
    where
        splitInput = splitOn " " input


parseFileSystem :: [String] -> FileSys -> FileSys
parseFileSystem list dir
    | (head list) == 

partOne :: [String] -> Int
partOne [] = 0
partOne (l:list)
    | head l == '$' = 0 + (partOne list)
    | otherwise = (getFileSize (parseFileSystemLine l)) + (partOne list)
    where
        getFileSize :: FileSys -> Int
        getFileSize (File name size) = size
        getFileSize other = 0

partTwo :: [String] -> Int
partTwo lines = 0

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let fileLines = lines content
    print "Part One:"
    print (partOne fileLines)
    print "Part Two:"
    print (partTwo fileLines)