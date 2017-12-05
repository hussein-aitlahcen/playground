module Lib where

import Control.Monad
import Data.List
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS

type Level = Input -> Output
type Input = BS.ByteString
type Output = (Int, Int)

contentOf :: String
          -> IO Input
contentOf = BS.readFile . ("./files/" ++)

day4 :: Level
day4 content = (part1, part2)
  where
    lines = C.split '\n'
    nonEmpty = filter (/= BS.empty)
    validLength f = length . filter (isValid f) . nonEmpty . lines $ content
    isValid projection line = length original == (S.size . uniques) original
      where
        words = C.split ' '
        original = projection . words $ line
        uniques = S.fromList
    part1 = validLength id
    part2 = validLength (map BS.sort)

day5 :: Level
day5 content = (0, 0)

days :: [(String, Level)]
days = [("day4", day4), ("day5", day5)]

run :: IO ()
run = join $ mapM_ putStrLn <$> mapM (\(n, f) -> ((++) (n ++ " -> ") . show) . f <$> contentOf (n ++ ".txt")) days
