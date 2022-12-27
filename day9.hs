import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List (transpose, findIndex)
import Data.List.Split (chunksOf, splitPlaces)
import Data.Semigroup (stimes)
import qualified Data.Set (fromList, toList, union, size)
import ParserCombinator as P


data Move = Move Int Int deriving (Show, Ord, Eq)

type Head = Move
type Tail = Move
type History = [[Move]]
type Moves = [Move]

data Action = Action Moves History Head [Tail]
    deriving (Show)

instance Semigroup Move where
    Move x1 y1 <> Move x2 y2 = Move (x1 + x2) (y1 + y2)

instance Monoid Move where
    mempty = Move 0 0

(<->) h (Move x y)= h <> (Move (-x) (-y))

parseUp = do
    _ <- P.char 'U' >> P.space
    m <- P.int
    return $ splitMoves $ Move 0 m

parseDown = do
    _ <- P.char 'D' >> P.space
    m <- P.int
    return $ splitMoves $ Move 0 (-m)

parseLeft = do
    _ <- P.char 'L' >> P.space
    m <- P.int
    return $ splitMoves $ Move (-m) 0

parseRight = do
    _ <- P.char 'R' >> P.space
    m <- P.int
    return $ splitMoves $ Move m 0

parseMove = parseUp P.<|> parseDown P.<|> parseLeft P.<|> parseRight

parse = fmap mconcat $ P.sepBy1 parseMove P.newline

main = do
    input1 <- load (P.run parse) "data/day9"
    print $ case input1 of
        (_, Right ms) -> part1 ms

    input2 <- load (P.run parse) "data/day9"
    print $ case input2 of
        (_, Right ms) -> part2 ms

initialPos :: Move
initialPos = mempty

initialHis n = stimes n [[initialPos]]

maxnorm (Move dx dy) = max (abs dx) (abs dy)

-- "normalize"
normalize (Move dx dy) = Move ((signum dx) * (min 1 (abs dx))) ((signum dy) * (min 1 (abs dy)))

splitMoves (Move x y) = stimes n $ [Move (x `div` n) (y `div` n)]
    where
        n = maxnorm (Move x y)

moveTail h t = if maxnorm (h <-> t) >= 2
                then t <> (normalize $ h <-> t)
                else t

moveTails _ []      = []  
moveTails h (t:ts)  = t':(moveTails t' ts)
    where
        t' = moveTail h t

move (Action [] his h t) = his
move (Action (m:ms) his h ts) = move $ Action ms his' h' ts'
    where
        h'      = h <> m
        ts'     = moveTails h' ts
        his'    = (zipWith (:) ts' his)

solution n ms = map Data.Set.size $ map Data.Set.fromList $ map reverse his
    where
        his = move $ Action ms (initialHis n) initialPos (stimes n $ [initialPos])

part1 = solution 1

part2 = solution 9