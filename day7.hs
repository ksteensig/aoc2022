import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List (transpose)
import Data.List.Split (splitPlaces)
import qualified Data.Set (fromList)
import ParserCombinator as P

type Size = Int
type Name = String
data File = File Name Size | Dir Name [File] Size deriving (Show)

isfile (File _ _)   = True
isfile (Dir _ _ _)  = False

soc         = P.char '$' -- start of cmd
cd          = P.string "cd"
ls          = P.string "ls"
dir         = P.string "dir"
name        = P.many1 $ P.nchar '\n'

computeSize (File _ s) = s
computeSize (Dir _ fs _) = sum $ map computeSize fs 

dirline = do
    _ <- dir >> P.space
    n <- name
    return $ Dir n [] 0 -- only need to know it's a dir

fileline = do
    size    <- P.int
    _       <- P.space
    n       <- name
    return $ File n size 

forward = do
    _ <- soc >> P.space
    _ <- cd >> P.space
    d <- name
    _ <- P.newline
    _ <- soc >> P.space
    _ <- ls
    return d

backward = do
    _ <- soc >> P.space
    _ <- cd >> P.space
    d <- P.string ".."
    return d

parseContents = fmap (filter isfile) $ P.sepBy1 (dirline P.<|> fileline) P.newline

parseDirLevel = do
    dname   <- forward
    files   <- P.newline >> parseContents
    dirs    <- P.many $ P.newline >> parseDirLevel
    _       <- P.newline >> backward
    return $ Dir dname (files ++ dirs) $ sum $ map computeSize $ dirs ++ files


parseDirLevel' = do
    dname   <- forward
    files   <- P.newline >> parseContents
    dirs    <- P.many $ P.newline >> parseDirLevel
    dirs'   <- P.many $ P.newline >> parseDirLevel'
    return $ Dir dname (files ++ dirs ++ dirs') $ sum $ map computeSize $ dirs ++ dirs' ++ files

parse = parseDirLevel'

main = do
    input <- load (P.run parse) "data/day7"
    print $ case input of
        (_, Right tree) -> part1 tree
    -- print $ case input of
    --     (_, Right tree) -> part2 tree

findLessThan _ (File name size) = 0
findLessThan thresh (Dir name files size)
    | size < thresh = (+) size $ sum $ map (findLessThan thresh) files
    | otherwise     =  sum $ map (findLessThan thresh) files

thresh = 100000
part1 tree = findLessThan thresh tree

-- required = 70000000
-- findSmallest _ size' (File name size) = size' 
-- findSmallest occupied size' (Dir name files size)
--     | (required - occupied <= size) && (size < size')   = min size $ minimum $ map (findSmallest occupied size) files
--     | otherwise                                         = min size' $ minimum $ map (findSmallest occupied size') files

-- part2 (Dir name files size) = findSmallest size size (Dir name files size)