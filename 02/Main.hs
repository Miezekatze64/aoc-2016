module Main where

data Dir = UP | DOWN | RIGHT | LEFT deriving (Show, Enum, Eq)
data Button = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | A | B | C | D deriving (Enum, Eq)
instance Show Button where
    show N1 = "1"
    show N2 = "2"
    show N3 = "3"
    show N4 = "4"
    show N5 = "5"
    show N6 = "6"
    show N7 = "7"
    show N8 = "8"
    show N9 = "9"
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"

(%) :: Integral a => a -> a -> a
(%) = mod

toString :: Show a => [a] -> String
toString [] = []
toString list = ( show $ head list ) ++ ( toString $ tail list )

part1 :: Button -> String -> String
part1 start dirs = toString $ solve1 ( map (parse) $ lines dirs ) start

part2 :: Button -> String -> String
part2 start dirs = toString $ solve2 ( map (parse) $ lines dirs ) start

solve1 :: [[Dir]] -> Button -> [Button]
solve1 [] start = []
solve1 list start = number : solve1 ( tail list ) number
    where number = fst $ part1' (start, head list)

solve2 :: [[Dir]] -> Button -> [Button]
solve2 [] start = []
solve2 list start = number : solve2 ( tail list ) number
    where number = fst $ part2' (start, head list)

parse :: String -> [Dir]
parse str = map (\x ->
    case x of
        'D' -> DOWN
        'U' -> UP
        'R' -> RIGHT
        'L' -> LEFT
    ) str

part1' :: (Button, [Dir]) -> (Button, [Dir])
part1' (pos, []) = (pos, [])
part1' (pos, dirs) = part1' (next1 dir pos, tail dirs) 
    where dir = head dirs

part2' :: (Button, [Dir]) -> (Button, [Dir])
part2' (pos, []) = (pos, [])
part2' (pos, dirs) = part2' (next2 dir pos, tail dirs) 
    where dir = head dirs

next1 :: Dir -> Button -> Button
next1 UP     x = if x == N3 || x == N2 || x == N1 then x else pred $ pred $ pred x
next1 DOWN   x = if x == N7 || x == N8 || x == N9 then x else succ $ succ $ succ x
next1 RIGHT  x = if x == N3 || x == N6 || x == N9 then x else succ x
next1 LEFT   x = if x == N1 || x == N4 || x == N7 then x else pred x

next2 :: Dir -> Button -> Button
next2 UP x = case x of
    N1 -> N1
    N2 -> N2
    N3 -> N1
    N4 -> N4
    N5 -> N5
    N6 -> N2
    N7 -> N3
    N8 -> N4
    N9 -> N9
    A -> N6
    B -> N7
    C -> N8
    D -> B

next2 DOWN x = case x of
    N1 -> N3
    N2 -> N6
    N3 -> N7
    N4 -> N8
    N5 -> N5
    N6 -> A
    N7 -> B
    N8 -> C
    N9 -> N9
    A -> A
    B -> D
    C -> C
    D -> D

next2 RIGHT x = case x of
    N1 -> N1
    N2 -> N3
    N3 -> N4
    N4 -> N4
    N5 -> N6
    N6 -> N7
    N7 -> N8
    N8 -> N9
    N9 -> N9
    A -> B
    B -> C
    C -> C
    D -> D

next2 LEFT x = case x of
    N1 -> N1
    N2 -> N2
    N3 -> N2
    N4 -> N3
    N5 -> N5
    N6 -> N5
    N7 -> N6
    N8 -> N7
    N9 -> N8
    A -> A
    B -> A
    C -> B
    D -> D


main :: IO ()
main = do
    input <- readFile "./input.txt"
    putStrLn ( "Part 1: " ++ part1 N5 input )
    putStrLn ( "Part 2: " ++ part2 N5 input )
