module Lib where

import Data.List
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS

contentOf :: String
          -> IO BS.ByteString
contentOf = BS.readFile . ("./files/" ++)

isValid :: ([BS.ByteString] -> [BS.ByteString])
        -> BS.ByteString
        -> Bool
isValid projection line = length original == (S.size . uniques) original
  where
    words = C.split ' '
    original = projection . words $ line
    uniques = S.fromList

day4 :: BS.ByteString -> (Int, Int)
day4 content = (part1, part2)
  where
    lines = C.split '\n'
    nonEmpty = filter (/= BS.empty)
    validLength f = length . filter (isValid f) . nonEmpty . lines $ content
    part1 = validLength id
    part2 = validLength (map BS.sort)

run :: IO ()
run = day4 <$> contentOf "day4.txt"  >>= print
