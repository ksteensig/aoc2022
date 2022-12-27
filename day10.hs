import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List (transpose, findIndex)
import Data.List.Split (chunksOf, splitPlaces)
import Data.Semigroup (stimes)
import qualified Data.Set (fromList, toList, union, size)
import ParserCombinator as P

data Instruction    = A Int | N deriving (Show)

data MicroOp        = MICROA1 | MICROA2 Int | MICRON deriving (Show)
type Register       = Int
type XReg           = Register
type Cycle          = Register
type History        = [XReg]
data Interpreter    = I [MicroOp] Cycle XReg deriving (Show)

type Width          = Int
type Height         = Int
data Cursor         = C Width Height
data CRT            = CRT [String]

parseAdd = do
    _ <- P.string "addx" >> P.space
    n <- P.int P.<|> P.int'
    return $ A n

parseNoop = do
    _ <- P.string "noop"
    return $ N

parseInstruction = parseAdd P.<|> parseNoop

parse = P.sepBy1 parseInstruction P.newline

main = do
    input1 <- load (P.run parse) "data/day10"
    print $ case input1 of
        (_, Right ops) -> part1 ops
    input2 <- load (P.run parse) "data/day10"
    mapM_ putStrLn $ case input2 of
        (_, Right ops) -> part2 ops

translate (A n) = [MICROA1, MICROA2 n]
translate N     = [MICRON]

interpret' ((I (MICROA1:mops) c x), his)        = (,) (I mops (c+1) x)      (x:his)
interpret' ((I ((MICROA2 n):mops) c x), his)    = (,) (I mops (c+1) (x+n))  (x:his)
interpret' ((I (MICRON:mops) c x), his)         = (,) (I mops (c+1) x)      (x:his)

interpret ((I [] c x), his)                     = (,) (I [] c x) his
interpret (instr, his)                          = interpret $ interpret' (instr, his)

initialHis      = [1]
initialXReg     = 1
initialCycle    = 0

part1 ops = sum [20*c20,60*c60,100*c100,140*c140,180*c180,220*c220] 
    where
        mops    = concat $ map translate ops
        his     = reverse $ snd $ interpret ((I mops initialCycle initialXReg), initialHis)
        c20     = his !! (20)
        c60     = his !! (60)
        c100    = his !! (100)
        c140    = his !! (140)
        c180    = his !! (180)
        c220    = his !! (220)

width   = 40
height  = 6

drawRow' x xreg row
     | left == x    = '#':row
     | mid == x     = '#':row
     | right == x   = '#':row
     | otherwise    = '.':row
    where
        (left, mid, right) = (xreg-1, xreg, xreg+1)

draw cycle xreg (CRT ([]))      =   CRT $ [drawRow' cycle xreg []]
draw cycle xreg (CRT (r:rs))    =   if x == 0 then CRT $ r'':r:rs
                                    else CRT $ r':rs
    where
        x   = cycle `mod` width
        r'  = drawRow' x xreg r
        r'' = drawRow' x xreg []


interpret2 ((I [] c x), his) (CRT rs)   = rs
interpret2 (instr, his) crt             = interpret2 (interpret' (instr, his)) rs'
    where
        (I _ c x) = instr
        rs' = draw c x crt

part2 ops = (map reverse) . reverse $ interpret2 ((I mops initialCycle initialXReg), initialHis) (CRT [])
    where
        mops = concat $ map translate ops