{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 as AC
import Data.Attoparsec.Combinator
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString as BS

type Level = Input -> IO Output
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
                 return (part1, part2)
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
                 return (part1, part2)
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

day6 :: Level
day6 content = return (0, 0)

data FlatTree = FlatTree { identifier :: BS.ByteString, weight :: Int, childrens :: [BS.ByteString] } deriving (Show)

instance Eq FlatTree where
  (==) a b = identifier a == identifier b

-- ssqhzgo (183) -> idwscr, jwmobb
day7 :: Level
day7 content = let x = mapM tree . nonEmptyLines $ content
               in case x of
                    Right trees -> do
                      let
                        identifiers = map identifier trees
                        stupids = join $ map childrens trees
                        Just x = find (not . (flip elem) stupids) identifiers
                      print x
                      return (0, 0)
                    Left err -> error err
  where
    readIdent = AC.takeWhile1 isLetter
    readWeight = char '(' *> decimal <* char ')'
    readChilds = AC.takeWhile (not . isLetter) *> AC.takeWhile isLetter `AC.sepBy` string ", "
    readTree = do
      ident <- readIdent
      char ' '
      w <- readWeight
      childs <- readChilds
      return $ FlatTree ident w childs
    tree = parseOnly readTree

days :: [(String, Level)]
days = [("day7", day7)]

run :: IO ()
run = join $ mapM_ putStrLn <$> mapM (\(n, f) -> ((++) (n ++ " -> ") . show <$>) . f =<< contentOf (n ++ ".txt")) days
