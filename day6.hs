import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List.Split (divvy)
import qualified Data.Set (fromList, toList, isSubsetOf)

parse = lines

main = do
  input <- load parse "data/day6"
  print input
  print $ map part1 input
  print $ map part2 input

part1 = solve 4
part2 = solve 14

solve distinct input = distinct + initial - after
    where
        initial = length . (map (length . Data.Set.toList . Data.Set.fromList)) . divvy distinct 1 $ input
        after   = length . dropWhile ((>)distinct) . (map (length . Data.Set.toList . Data.Set.fromList)) . divvy distinct 1 $ input
