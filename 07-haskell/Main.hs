import Data.List.Split
import Data.List

check1 :: String -> Bool
check1 str = length str > 4 && (any isAbba outside && not (any isAbba inside))
  where
    isAbba  str = any isAbba' ( chunksOf 4 str ++ chunksOf 4 (tail str) ++ chunksOf 4 (tail $ tail str) ++ chunksOf 4 (tail $ tail $ tail str) )
    isAbba' str = length str >= 4 && (
      let (a:b:c:d:xs) = str
      in a == d && b == c && a /= b)
    outside = map snd . fst . partition (even . fst) $ zip [0..] list
    inside  = map snd . fst . partition (odd . fst)  $ zip [0..] list
    list    = concatMap (splitOn "]") $ splitOn "[" str

part1 :: String -> Int
part1 = length . filter check1 . lines

check2 :: String -> Bool
check2 str = res
  where
    res        = any (\x -> aba2bab x `elem` bab) aba
    
    aba2bab (a:b:c) = [b, a, b]
    bab        = concatMap aba' inside
    
    aba        = concatMap aba' outside
    aba'   str = filter isAba ( chunksOf 3 str ++ chunksOf 3 (tail str) ++ chunksOf 3 (tail $ tail str))
    isAba str  = length str >= 3 && (
      let (a:b:c:xs) = str
      in a == c && a /= b)
    
    outside = map snd . fst . partition (even . fst) $ zip [0..] list
    inside  = map snd . fst . partition (odd . fst)  $ zip [0..] list
    list    = concatMap (splitOn "]") $ splitOn "[" str

part2 :: String -> Int
part2 = length . filter check2 . lines

main :: IO ()
main = do
  input <- readFile "input"
  print $ part1 input
  print $ part2 input
