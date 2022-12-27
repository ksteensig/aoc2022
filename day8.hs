import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List (transpose, findIndex)
import Data.List.Split (chunksOf, splitPlaces)
import qualified Data.Set (fromList, toList, union, size)
import ParserCombinator as P

type Coord  = (Int, Int)
type Height = Int
data Tree   = Tree Coord Height deriving (Show, Eq, Ord)

coordinates w = [(x,y) | x <- [0..], y <- [0..w-1]]

higherThan (Tree _ h) (Tree _ h') = h >= h' 
shorterThan t t' = not $ higherThan t t'

parseTreeRow = (fmap . map) readInt . fmap (chunksOf 1) $ P.many1 digit

toTree ((x,y), h) = Tree (x, y) h

parseTreeRows = P.sepBy1 parseTreeRow P.newline

main = do
    input <- load (P.run parseTreeRows) "data/day8"
    print $ case input of
        (_, Right trees) -> part1 $ createTrees trees
    print $ case input of
        (_, Right trees) -> part2 $ createTrees trees

createTrees trees = (map . map) toTree $ zipWith zip coords trees
    where
        w   = length $ head trees
        coords  = chunksOf w $ coordinates w

visible [] = []
visible (t:ts) = t : visible (filter (shorterThan t) ts)

lookLeft = Data.Set.fromList . concat . map visible
lookRight = lookLeft . map reverse 
lookTop = lookLeft . transpose
lookBottom = lookRight . transpose

part1 trees = Data.Set.size $ lookLeft trees `Data.Set.union` lookRight trees `Data.Set.union` lookTop trees `Data.Set.union` lookBottom trees 

score1D' ([], (Tree c h):right) = 0
score1D' (left, (Tree c h):[]) = 0
score1D' (left, (Tree c h):right) = score * score'
    where
        score = case findIndex (\(Tree c' h') -> h' >= h) $ reverse left of
                Nothing     -> length left
                Just i      -> i + 1
        score' = case findIndex (\(Tree c' h') -> h' >= h) right of
                Nothing     -> length right
                Just i      -> i + 1

score1D ts = map score1D' $ map ($ ts) $ map splitAt [0..w-1]
    where
        w = length ts

part2 ts = maximum . map maximum $ zipWith (zipWith (*)) vertical horizontal
    where
        vertical = transpose $ map score1D $ transpose ts
        horizontal = map score1D ts