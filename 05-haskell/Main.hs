module Main where

import Data.Char
import Data.List
import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Builder
import Data.Maybe

hashStr :: String -> String
hashStr = LB.unpack . toLazyByteString . byteStringHex . Crypto.Hash.MD5.hash . B.pack

part1 :: String -> String
part1 prefix = map (!! 5) $ take 8 $ filter (isPrefixOf "00000") $ map (hashStr . \a -> prefix ++ show a) [0..]

part2 :: String -> String
part2 prefix = map (snd . fromJust . \i -> find (\(a, _) -> a == intToDigit i)  (map (\x -> (x !! 5, x !! 6)) $ filter (isPrefixOf "00000") $ map (hashStr . \a -> prefix ++ show a) [0..])) [0..7]

main :: IO ()
main = do
  putStrLn $ part1 prefix
  putStrLn $ part2 prefix
  where
    -- prefix = "abc" -- SAMPLE
    prefix = "reyedfim"
