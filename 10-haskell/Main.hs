module Main where
import Data.List
import Data.Maybe
import Control.Monad
import Data.Bifunctor

type Value    = Int
type Index    = Int
data Dest     = Bot Index | Output Index deriving (Show, Eq)

type Bot   = ((Maybe Value, Maybe Value), (Dest, Dest))
type Start = (Value, Index)

destIndex :: Dest -> Index
destIndex (Bot a)    = a
destIndex (Output a) = a

parse :: [String] -> ([Bot], [Start])
parse = first s . parse'
  where
    s = map snd . sortOn fst

parse' :: [String] -> ([(Index, Bot)], [Start])
parse' []     = ([], [])
parse' (x:xs) = bimap (f ++) (s ++) ys
  where
    f = maybeToList f'
    s = maybeToList s'

    f' :: Maybe (Index, Bot)
    s' :: Maybe Start

    s' = case head w of
      "value" -> let val = read (w !! 1)
                     bot = read (w !! 5)
                 in
                  Just (val, bot)
      _       -> Nothing

    f' = case head w of
      "bot" -> let bot  = read (w !! 1)
                   low  = parseOut (w !! 5) $ read (w !! 6)
                   high = parseOut (w !! 10) $ read (w !! 11)

                   parseOut "output" = Output
                   parseOut "bot"    = Bot
               in
                  Just (bot, ((Nothing, Nothing), (low, high)))
      _       -> Nothing

    w  = words x
    ys = parse' xs


unpairMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
unpairMaybe (Just (a, b)) = (Just a, Just b)
unpairMaybe Nothing       = (Nothing, Nothing)

unpairList :: [(a, b)] -> ([a], [b])
unpairList l = (map fst l, map snd l)

fill :: [Bot] -> [Start] -> [Bot]
fill bots values = zipWith (\i (_, xs) -> (
                               (\xs -> if not (null xs) then (Just (head xs), if not $ null $ tail xs then Just (xs !! 1) else Nothing) else (Nothing, Nothing)) $ map (\x -> fst (values !! x)) $ findIndices (\(_, ind) -> ind == i) values
                               , xs)) [0..] bots

type Output = (Index, Value)
step :: ([Bot], [Output]) -> ([Bot], [Output])
step (bots, outputs) = (nbots, sort (outputs ++ outs))
  where
    nbots             = zipWith (\val (_, ind) -> (val, ind)) nbots' bots
    nbots'            = map (toTuple . map (snd . (list !!)) .
                             (\x -> elemIndices (Bot x) (map fst list))) [0..(length bots-1)]
    outs              = concatMap (map (first destIndex . (list !!)) . \x -> elemIndices (Output x) (map fst list)) [0..maxOut]

    toTuple (a:b:_)   = (Just a, Just b)
    toTuple [a]       = (Just a, Nothing)
    toTuple []        = (Nothing, Nothing)

    maxOut            = fromMaybe 0 $ join $ listToMaybe $ reverse $ sort $ map (output . fst) list
    output (Output a) = Just a
    output _          = Nothing

    list              = concatMap step' $ zip [0..] bots

step' :: (Index, Bot) -> [(Dest, Value)]
step' (_, ((Nothing, Nothing), _)) = []
step' (i, ((Nothing, Just a), _)) = [(Bot i, a)]
step' (i, ((Just a, Nothing), _)) = [(Bot i, a)]
step' (_, ((Just v1, Just v2), (d1, d2))) = [(d1, min v1 v2), (d2, max v1 v2)]

getBot :: [Bot] -> [Start] -> (Int, Int) -> Index
getBot bots values (a, b) = error $ show (botsA, botsB)
  where

    botsA  = allBots (Bot startA) []
    botsB  = allBots (Bot startB) []

    allBots (Bot    index) xs = if any (\x -> destIndex x `elem` xs) vals then xs else concatMap (\x -> allBots x (index:xs)) vals
      where
        vals = (\(a, b) -> [a, b]) $ snd (bots !! index)
    allBots (Output index) xs = xs

    (startA, startB) = (startBots a, startBots b)
    startBots i      = fromMaybe (-1) $ snd $ unpairMaybe $ find (\x -> i == fst x) values

main :: IO ()
main = do
  vals <- parse . lines <$> readFile "./input"
  return ()
