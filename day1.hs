import AOC2022 (load, readInt)
import Data.List (sort, tails, transpose)
import Data.List.Split (splitWhen)
import System.IO (openFile)

parse = (map . map) readInt . splitWhen (== "") . lines

main = do
  list <- load parse "data/day1"
  print $ map ($ list) [part1, part2]

solution n = sum . take n . reverse . sort . map sum

part1 = solution 1

part2 = solution 3