import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List (transpose)
import Data.List.Split (splitPlaces)
import qualified Data.Set (fromList)
import ParserCombinator as P

data Field = Occupied String | Empty
  deriving (Show, Eq)

type Row = [Field]

type Rows = [Row]

data Move = Move Int Int Int
  deriving (Show)

parseEmpty = P.space >> P.space >> P.space >> return Empty

parseOccupied = do
  _ <- P.char '['
  c <- P.any
  _ <- P.char ']'
  return $ Occupied [c]

parseField = parseOccupied <|> parseEmpty

parseRow = do
  fs     <- P.sepBy1 parseField P.space
  return $  fs


-- transpose rows to columns, remove Empty tops from each column
parseColumns = (fmap . fmap) (dropWhile (== Empty)) $ fmap transpose $ do
  rs    <- P.sepBy1 parseRow P.newline
  return rs

parseDigitsRow = do
  _     <- P.space
  ds    <- P.sepBy1 P.int (P.space >> P.space >> P.space)
  _     <- P.space
  return ds

parseMove = do
  _       <- P.string "move"
  _       <- P.space
  amount  <- P.int
  _       <- P.space
  _       <- P.string "from"
  _       <- P.space
  from    <- P.int
  _       <- P.space
  _       <- P.string "to"
  _       <- P.space
  to      <- P.int
  return $ Move amount from to

parseMoves = P.sepBy1 parseMove P.newline

parse = do
  columns <- parseColumns
  _       <- P.newline
  digits  <- parseDigitsRow
  _       <- P.newline
  _       <- P.newline
  moves   <- parseMoves
  return $ (digits, columns, moves)

main = do
  input <- load (run parse) "data/day5"
  print $ case input of
    (_, Right (ds, cs, ms)) -> map (\(Occupied s) -> head s) $ map head $ foldl part1 cs ms 
  print $ case input of
    (_, Right (ds, cs, ms)) -> map (\(Occupied s) -> head s) $ map head $ foldl part2 cs ms 


part1 cols (Move amount from to) = cols''
  where
    (first, from':rest) = splitAt (from-1) cols
    vals = reverse $ take amount from'
    from'' = drop amount from'
    
    cols' = (first ++ from'':rest)

    (first', to':rest') = splitAt (to-1) cols'
    to'' = vals ++ to'
  
    cols'' = (first' ++ to'':rest')

  
part2 cols (Move amount from to) = cols''
  where
    (first, from':rest) = splitAt (from-1) cols
    vals = take amount from'
    from'' = drop amount from'
    
    cols' = (first ++ from'':rest)

    (first', to':rest') = splitAt (to-1) cols'
    to'' = vals ++ to'
  
    cols'' = (first' ++ to'':rest')