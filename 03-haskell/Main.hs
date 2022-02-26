{-# Language OverloadedStrings #-}
module Main where

import Data.List
import qualified Data.Text as T
type Triangle = (Int, Int, Int) 

part1 :: T.Text -> Int
part1 = length . filter isValid . parse

part2 :: T.Text -> Int
part2 = length . filter isValid . parse2

isValid :: Triangle -> Bool
isValid (a, b, c) = a + b > c && b + c > a && c + a > b

parse :: T.Text -> [Triangle]
parse = map (toTriangle . filter (\x -> T.length x > 0) . T.splitOn " " . T.strip ) . T.lines

group' :: Int -> [a] -> [[a]]
group' n = uncurry (:) . foldr (\x (l,r) -> if length l == n then ([x],l:r) 
                                                            else (x:l,r)) ([], [])
parse2 :: T.Text -> [Triangle]
parse2 = toTriangleList . filter (\x -> T.length x > 0) . map T.strip . T.splitOn " " . T.replace "\n" " "

toTriangleList :: [T.Text] -> [Triangle]
toTriangleList = map toTriangle . group' 3 . concat . transpose . group' 3

toTriangle :: [T.Text] -> Triangle
toTriangle [a, b, c] = (read $ T.unpack a, read $ T.unpack b, read $ T.unpack c)

main :: IO ()
main = do
    input <- readFile "./input.txt"
    putStrLn ( "Part 1: " ++ ( show $ part1 $ T.pack input ) )
    putStrLn ( "Part 2: " ++ ( show $ part2 $ T.pack input ) )
