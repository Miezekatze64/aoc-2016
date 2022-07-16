module Main where
import Data.List
import Data.Char
import qualified Data.Ord

type Room = (String, Int, String)

parse :: String -> [Room]
parse = map parse' . lines

parse' :: String -> Room
parse' str = (name, roomId, checkSum)
  where
    name     =        reverse $ tail $ dropWhile (/= '-') $ reverse str
    checkSum =        reverse $ tail $ takeWhile (/= '[') $ reverse str
    roomId   = read $ reverse $ tail $ takeWhile (/= '-') $ dropWhile (/= '[') $ reverse str

isReal :: Room -> Bool
isReal room = real
  where
    real = take 5 ( concatMap (map head . sort) $ groupBy (\a b -> length a == length b) $ reverse $ sortOn length $ group $ sort $ filter (/= '-') name) == check
    name              = [ x | x <- name', x /= '-']
    (name', _, check) = room

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

solve :: String -> Int
solve input = sum $ map snd' $ filter isReal $ parse input

decipher :: String -> String
decipher = unlines . map decipher' . lines

decipher' :: String -> String
decipher' str = show id ++ ": " ++ map (\c -> if c == '-' then ' ' else shift id c) name
  where
    shift x c     = chr $ (((ord c - 97) + x) `mod` 26) + 97
    (name, id, _) = parse' str

main :: IO ()
main = do
  input <- readFile "./input"
  putStrLn "-- PART 1 --"
  print $ solve input
  putStrLn "-- PART 2 --"
  putStrLn $ decipher input
