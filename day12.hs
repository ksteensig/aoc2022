import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import Data.List (transpose, sort, elemIndices)
import Data.List.Split (chunksOf, splitPlaces)
import qualified Data.Set (fromList, toList, union, size)
import ParserCombinator as P
import Algorithm.Search (aStar)
import Debug.Trace (trace)

data Point = P Char Int Int deriving (Show, Eq, Ord)

getChar' (P c _ _) = c

data Grid = G [[Point]] deriving (Show)

parseStart = P.char 'S'
parseEnd = P.char 'E'

parseRow = P.many1 $ parseStart P.<|> parseEnd P.<|> P.nchar '\n'

coordinates grid' = chunksOf w $ [(P (grid' !! y !! x) x y) | y <- [0..h-1], x <- [0..w-1]]
    where
        w = length $ head grid'
        h = length grid'

parse = do
    rows <- P.sepBy1 parseRow P.newline
    return $ rows

main = do
    input1 <- load (P.run parse) "data/day12"
    print $ case input1 of
        (_, Right grid) -> part1 (G $ coordinates grid)
    input2 <- load (P.run parse) "data/day12"
    print $ case input2 of
        (_, Right grid) -> part2 (G $ coordinates grid)


c   `ht` 'S'                                   = True
'E' `ht` c                                     = True
'S' `ht` c
        | not $ elem c ['a', 'b']           = False
        | otherwise                         = True
c   `ht` 'E'
        | not $ elem c ['y', 'z']           = False
c   `ht` c'
        | (fromEnum c') - (fromEnum c) >= 2 = False
        | otherwise                         = True

move (dx, dy) (x, y) = (x+dx, y+dy)

left = move (-1, 0)
right = move (1, 0)
up = move (0, -1)
down = move (0, 1)


climbable (G grid) (P c x y) (x', y')
    | (>=) x' (length $ head grid)  = False
    | (<) x' 0                      = False
    | (>=) y' (length grid)         = False
    | (<) y' 0                      = False
    | otherwise                     = c `ht` c'
    where
        (P c' _ _) = grid !! y' !! x'


neighbors (G grid) (P c x y) = map (\(x',y') -> grid !! y' !! x') $ filter (climbable (G grid) (P c x y)) [l, r, u, d]
    where
        l = left (x, y)
        r = right (x, y)
        u = up (x, y)
        d = down (x, y)

dist (P _ _ _) (P _ _ _) = 1

isStart1 (P c _ _) = c == 'S'
isEnd (P c _ _) = c == 'E'

getStart1 (G c) = [ (x,y) | (y,line) <- zip [0..] ((map . map) isStart1 (c)), x <- elemIndices True line ]
getEnd (G c) = [ (x,y) | (y,line) <- zip [0..] ((map . map) isEnd (c)), x <- elemIndices True line ]

part1 (G grid) = case aStar (neighbors (G grid)) dist (dist end) ((==) end) start of
                    Nothing -> undefined
                    Just (i, _) -> i
    where
        (sx, sy):_  = getStart1 (G grid)
        (ex, ey):_  = getEnd (G grid)

        start       = grid !! sy !! sx
        end         = grid !! ey !! ex

isStart2 (P c _ _) = c == 'S' || c == 'a'
getStart2 (G c) = [ (x,y) | (y,line) <- zip [0..] ((map . map) isStart2 (c)), x <- elemIndices True line ]

part2 (G grid) = head . sort . catMaybes . (fmap . fmap) fst $ map (aStar (neighbors (G grid)) dist (dist end) ((==) end)) start
    where
        ss    = getStart2 (G grid)
        (ex, ey):_    = getEnd (G grid)

        start       = map (\(sx, sy) -> grid !! sy !! sx) ss
        end       = grid !! ey !! ex