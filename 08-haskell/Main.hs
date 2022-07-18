module Main where

import Data.List.Split (splitOn)
import Data.List

type Image = [[Bool]]
data Inst  = Rect (Int, Int) | RotRow (Int, Int) | RotCol (Int, Int) deriving (Show)

parse' :: String -> Inst
parse' str = case head w of
    "rect"     -> Rect (read $ head $ splitOn "x" $ last w, read $ last $ splitOn "x" $ last w)
    "rotate"   -> case w !! 1 of
      "row"    -> RotRow (read $ last $ splitOn "y=" $ w !! 2, read $ last w)
      "column" -> RotCol (read $ last $ splitOn "x=" $ w !! 2, read $ last w)
  where
    w = words str

parse :: String -> [Inst]
parse = map parse' . lines

applyInst :: Image -> Inst -> Image
applyInst img  (Rect (w, h))        = map (\as -> replicate w True ++ drop w as) (take h img) ++ drop h img
applyInst img  (RotRow (row, step)) = take row img ++ (\(a, b) -> b ++ a) (splitAt (length ( img !! row)-(step `mod` length ( img !! row))) (img !! row)) : drop (row+1) img
applyInst img_ (RotCol (col, step)) = reverse $ transpose (take col img ++ (\(a, b) -> b ++ a) (splitAt (step `mod` length (img !! col)) (img !! col)) : drop (col+1) img)
  where
    img = transpose $ reverse img_

createImage :: Int -> Int -> Image
createImage w h = replicate h $ replicate w False

step :: (Image, [Inst], Bool) -> (Image, [Inst], Bool)
step (img, x:xs, halt) = (applyInst img x, xs, halt)
step (img, [], __) = (img, [], True)

showImage :: Image -> IO ()
showImage = putStrLn . unlines . map (map (\x -> if x then 'O' else ' '))

solve :: String -> Image
solve str = (\(a, _, _) -> a) $ head $ dropWhile (\(_, _, a) -> not a) $ iterate step (createImage 50 6, parse str, False)

part1 :: String -> Int
part1 = sum . map (length . filter id) . solve

part2 :: String -> IO ()
part2 = showImage . solve

main :: IO ()
main = do
  input <- readFile "./input"
  putStrLn "PART 1:"
  print $ part1 input
  putStrLn "\nPART 2:"
  part2 input
