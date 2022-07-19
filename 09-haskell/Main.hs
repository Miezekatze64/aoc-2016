module Main where

import Data.Maybe
import Data.List
import Data.List.Split (splitOn)

decompress :: String -> String
decompress str = fst $ head $ dropWhile (\(_, a) -> a /= []) $ iterate step ([], str)

step :: (String, String) -> (String, String)
step (dc, xs) = case head xs of
  '(' -> let close       = fromJust $ elemIndex ')' $ tail xs
             (f, ys)     = (take close $ tail xs, drop (close+2) xs)
             (num:rep:_) = splitOn "x" f
        in
          (dc ++ concat (replicate (read rep) (take (read num) ys)), drop (read num) ys)

  a -> (dc++[a], tail xs)

part2 :: String -> Int
part2 str = fst $ head $ dropWhile (\(_, a) -> a /= []) $ iterate step2 (0, str)

step2 :: (Int, String) -> (Int, String)
step2 (num, str) = case head str of
  '(' ->
    let close = fromJust $ elemIndex ')' $ tail str
        (f, ys) = (take close $ tail str, drop (close + 2) str)
        (num_ : rep : _) = splitOn "x" f
     in (num + (part2 (take (read num_) ys) * read rep), drop (read num_) ys)
  a -> (num + 1, tail str)

main :: IO ()
main = do
  input <- readFile "input"
  putStr "PART 1: "
  print $ length $ decompress input
  putStr "PART 2: "
  print $ part2 input
