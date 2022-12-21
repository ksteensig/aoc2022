import AOC2022 (load, readInt)
import Data.List (sort, tails, transpose)
import Data.List.Split (chunksOf, splitOn)
import System.IO (openFile)

data Moves = Rock | Paper | Scissor
  deriving (Show)

data Game = Game Moves Moves
  deriving (Show)

data Result = Win | Lose | Draw
  deriving (Show)

translateMove "A" = Rock
translateMove "B" = Paper
translateMove "C" = Scissor
translateMove "X" = Rock
translateMove "Y" = Paper
translateMove "Z" = Scissor
translateMove _ = undefined

gameScore (Game Rock Rock) = 1 + 3
gameScore (Game Rock Paper) = 2 + 6
gameScore (Game Rock Scissor) = 3 + 0

gameScore (Game Paper Rock) = 1 + 0
gameScore (Game Paper Paper) = 2 + 3
gameScore (Game Paper Scissor) = 3 + 6

gameScore (Game Scissor Rock) = 1 + 6
gameScore (Game Scissor Paper) = 2 + 0
gameScore (Game Scissor Scissor) = 3 + 3

translateGame s s' = Game (translateMove s) (translateMove s')

parse = map ((\[m, m'] -> Game m m') . map translateMove . splitOn " ") . lines

main = do
  list <- load parse "data/day2"
  -- print $ fun list
  print $ part1 list

part1 = sum . map gameScore