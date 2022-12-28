import System.IO
import System.Environment
import Data.List.Split
import Data.List as List
import Data.Map as Map
import Data.HashSet as HashSet


data FileSys = File {fileName::String, fileSize::Int} | Directory {dirName::String, dirContents::(Map String FileSys)} deriving (Show, Eq)


addToDir :: FileSys -> FileSys -> FileSys
addToDir (Directory dName dCont) file = Directory{dirName = dName, dirContents = Map.insert (fileName file) file dCont}
addToDir f _ = f

getDirectoryLines :: [String] -> [String]
getDirectoryLines (l:lines) = l:(getDirectoryLines' lines 1)
    where 
        getDirectoryLines' [] _ = []
        getDirectoryLines' _ 0 = []
        getDirectoryLines' (l:lines) n
            | isPrefixOf "$ cd .." l = l:(getDirectoryLines' lines (n - 1))
            | isPrefixOf "$ cd" l = l:(getDirectoryLines' lines (n + 1))
            | isPrefixOf "$ ls" l = getDirectoryLines' lines n
            | otherwise = l:(getDirectoryLines' lines n)

parseFileSystemLine :: String -> FileSys
parseFileSystemLine input
    | head splitInput == "dir" = Directory (last splitInput) Map.empty
    | otherwise = File (last splitInput) (read (head splitInput)::Int)
    where
        splitInput = splitOn " " input

parseFileSystem :: [String] -> FileSys -> FileSys
parseFileSystem [] dir = dir
parseFileSystem (l:list) dir
    | isPrefixOf "$ cd .." l = dir
    | isPrefixOf "$ cd" l = parseFileSystem (List.drop (length childLines) list) (Directory (dirName dir) (Map.adjust (\x -> (parseFileSystem childLines x)) targetName (dirContents dir)))
    | isPrefixOf "$ ls" l = parseFileSystem list dir
    | otherwise = parseFileSystem list (addToDir parsedLine dir)
        where
            parsedLine = parseFileSystemLine l
            childLines = getDirectoryLines list
            targetName = List.drop 5 l


sumFileSizes :: [String] -> Int
sumFileSizes lines = sumFileSizes' lines [] HashSet.empty
    where
        sumFileSizes' (l:lines) stack set
        | isPrefixOf "$ cd .." l = sumFileSizes' total lines (init stack) set
        | isPrefixOf "$ cd " l = sumFileSizes' total lines (dirname:stack) (HashSet.insert dirname set) 
        | isPrefixOf "dir" l = sumFileSizes' total lines stack set
        |  = sumFileSizes' total lines stack set
        | otherwise = 0
        dirname = drop 5 l
        fileSize = read (head (splitOn " " l))::Int

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