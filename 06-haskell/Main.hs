module Main where
import Data.List

part1 :: String -> String
part1 str = map (head . last . sortOn length . group . sort) $ transpose $ lines str

part2 :: String -> String
part2 str = map (head . head . sortOn length . group . sort) $ transpose $ lines str

main :: IO ()
main = do
  input <- readFile "input"
  print $ part1 input
  print $ part2 input

