import AOC2022 (load)
import Data.Char (toUpper)
import Data.List.Split (chunksOf)
import qualified Data.Set (fromList, toList, intersection)
import qualified Data.Map (fromList, lookup)

parse input = (map . map) Data.Set.fromList $ map (\(l,s) -> chunksOf (l `div` 2) s) (zip (map length (lines input)) (lines input))

main = do
  list <- load parse "data/day3"
  print $ part1 list

points = Data.Map.fromList $ zip (concat [['a'..'z'], map toUpper ['a'..'z']]) [1..52]

part1 = sum . map sum . map (\(s1:s2:[]) -> (flip Data.Map.lookup) points . head . Data.Set.toList $ Data.Set.intersection s1 s2)