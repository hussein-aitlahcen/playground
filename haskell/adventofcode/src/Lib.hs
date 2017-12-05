{-# LANGUAGE BangPatterns #-}

module Lib where

import qualified Data.Sequence as SQ
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

nonEmpty :: [Input] -> [Input]
nonEmpty = filter (/= BS.empty)

nonEmptyLines :: Input -> [Input]
nonEmptyLines = nonEmpty . C.lines

day4 :: Level
day4 content = let part1 = validLength id
                   part2 = validLength $ map BS.sort
               in
                 (part1, part2)
  where
    validLength f = length . filter (isValid f) . nonEmptyLines $ content
    isValid projection line = length original == (S.size . uniques) original
      where
        words = C.split ' '
        original = projection . words $ line
        uniques = S.fromList

day5 :: Level
day5 content = let part1 = run id 0 0 maze
                   part2 = run  (\x -> if x >= 3 then -1 else 1) 0 0 maze
               in
                 (part1, part2)
  where
    maze = SQ.fromList . map (readInt . C.unpack) . nonEmptyLines $ content
    readInt x = read x :: Int
    run :: (Int -> Int) -> Int -> Int -> SQ.Seq Int -> Int
    run !f !offset !acc !stack
      | offset >= (SQ.length stack) || offset < 0 = acc
      | otherwise = let value = SQ.index stack offset
                        nextStack = SQ.update offset (value + f value) stack
                    in
                      run f (offset + value) (acc + 1) nextStack
      where
        sl = SQ.length

days :: [(String, Level)]
days = [("day4", day4), ("day5", day5)]

run :: IO ()
run = join $ mapM_ putStrLn <$> mapM (\(n, f) -> ((++) (n ++ " -> ") . show) . f <$> contentOf (n ++ ".txt")) days
