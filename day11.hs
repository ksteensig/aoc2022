import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List (transpose, sort)
import Data.List.Split (chunksOf, splitPlaces)
import Data.Semigroup (stimes)
import qualified Data.Set (fromList, toList, union, size)
import ParserCombinator as P


type Item       = Int
type Id         = Int
type TestTrue   = Id
type TestFalse  = Id
type Divisible  = Int

data Int'       = Const Int | Old deriving Show

data Operation  = PLUS Int' Int' | MUL Int' Int' deriving (Show)
data Test       = Test Divisible TestTrue TestFalse deriving (Show)
data Monkey     = Monkey Id [Item] Operation Test deriving (Show)

data Throw = Throw Id Item Id deriving (Show)

getId (Monkey id _ _ _) = id
setItems (Monkey id items op t) items' = (Monkey id items' op t)
getDiv (Monkey _ _ _ (Test d _ _)) = d

integer = P.int 

parseMonkeyId :: Parser Id
parseMonkeyId = do
    _   <- P.string "Monkey" >> P.space
    id  <- P.int
    _   <- P.char ':'
    return $ id

parseStartingItems :: Parser [Item]
parseStartingItems = do
    _       <- P.space >> P.space
    _       <- P.string "Starting items: "
    items   <- P.sepBy1 integer (P.char ',' >> P.space)
    return items

plus    = P.char '+'
mul     = P.char '*'

parseOldxOld = do
    _ <- P.string "old" >> P.space
    _ <- mul
    _ <- P.space >> P.string "old"
    return $ MUL Old Old

parseOldxConst = do
    _       <- P.string "old" >> P.space
    _       <- mul
    const   <- P.space >> integer
    return $ MUL Old (Const const)

parseOldpConst = do
    _       <- P.string "old" >> P.space
    _       <- plus
    const   <- P.space >> integer
    return $ PLUS Old (Const const)

parseOp' = parseOldxOld P.<|> parseOldxConst P.<|> parseOldpConst

parseOp :: Parser Operation
parseOp = do
    _       <- P.space >> P.space
    _       <- P.string "Operation: new = "
    op     <- parseOp'
    return op


parseTest = do
    _       <- P.space >> P.space
    _       <- P.string "Test: divisible by" >> P.space
    div'    <- integer
    _       <- P.newline >> P.space >> P.space >> P.space >> P.space
    _       <- P.string "If true: throw to monkey" >> P.space
    t       <- P.int
    _       <- P.newline >> P.space >> P.space >> P.space >> P.space
    _       <- P.string "If false: throw to monkey" >> P.space
    f       <- P.int
    return $ Test div' t f


parseMonkey = do
    id'     <- parseMonkeyId
    _       <- P.newline
    items   <- parseStartingItems
    _       <- P.newline
    op      <- parseOp
    _       <- P.newline
    test    <- parseTest

    return $ Monkey id' items op test

parse = P.sepBy1 parseMonkey (P.newline >> P.newline)

main = do
    input1 <- load (P.run parse) "data/day11"
    print $ case input1 of
        (_, Right ms) -> part1 ms
    input2 <- load (P.run parse) "data/day11"
    print $ case input2 of
        (_, Right ms) -> part2 ms

worry item (PLUS Old (Const c)) = item + c
worry item (MUL Old (Const c)) = item * c
worry item (MUL Old Old) = item * item

test item (Test d t f) = if item `mod` d == 0 then t else f 

inspect (Monkey _ [] _ _) _         = []
inspect (Monkey id (i:is) op t) d   = (Throw id i'' id'):(inspect (Monkey id is op t) d)
    where
        i'   = worry i op
        i''  =  d i'
        id' = test i'' t

distribute' ms (Throw idF item idT) = ms'
    where
        (msl, m:msr)            = splitAt idT ms
        (Monkey idT' is op t)   = m
        is'                     = reverse $ item:(reverse is)
        m'                      = (Monkey idT' is' op t)
        ms'                     = msl ++ (m':msr)

distribute ms []        = ms
distribute ms (t:ts)    = distribute  (distribute' ms t) ts

iteration' m ms tss d = (ms'', ts:tss)
    where
        ts                  = inspect m d
        ms'                 = distribute ms ts

        id                  = getId m
        (msl, m':msr)       = splitAt id ms'
        m''                 = setItems m' []

        ms''                = (msl ++ (m'':msr))

iteration idx ms' tss d
    | idx >= length ms' = (ms', reverse tss)
    | otherwise         = iteration (idx+1) ms'' tss' d
    where
        m               = ms' !! idx
        (ms'', tss')    = iteration' m ms' tss d


iterations 0 ms tss _   = (ms, tss)
iterations n ms tss d   = iterations (n-1) ms' tss'' d
    where
        (ms', tss')     = iteration 0 ms [] d
        tss''           = tss':tss
 
solution n d ms = foldl (*) 1 $ take 2 $ reverse $ sort $ map length $ foldl (zipWith (++)) t ts
    where
        (ms', t:ts) = iterations n ms [] d

part1 = solution 20 (flip div 3)

part2 ms = solution 10000 (flip mod d) ms
    where
        d = foldl (*) 1 $ map getDiv ms