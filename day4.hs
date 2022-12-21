import AOC2022 (load, readInt)
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import qualified Data.Set (fromList, toList, isSubsetOf)

parse = (map . map) (Data.Set.fromList . (\[p, p'] -> [p..p']) . map readInt . splitOn "-") . map (splitOn ",") . lines 

main = do
  list <- load parse "data/day4"
  print $ part1 list

part1 = sum . map fromEnum . map (\[p, p'] -> (Data.Set.isSubsetOf p p') || (Data.Set.isSubsetOf p' p))