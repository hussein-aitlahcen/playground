module Lib where

import Data.List
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS

contentOf :: String -> IO BS.ByteString
contentOf = BS.readFile . ("./files/" ++)

isValid :: BS.ByteString -> Bool
isValid line = length original == (S.size . uniques) original
  where
    words = C.split ' '
    sorted = map BS.sort
    original = sorted . words $ line
    uniques = S.fromList

day4 :: BS.ByteString -> Int
day4 content = length . valid . nonEmpty . lines $ content
  where
    valid = filter isValid
    lines = C.split '\n'
    nonEmpty = filter (/= BS.empty)

run :: IO ()
run = day4 <$> contentOf "day4.txt"  >>= print
