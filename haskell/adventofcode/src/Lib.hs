{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8            as C
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Internal         as BSI
import           Data.Char
import           Data.Either
import           Data.List
import qualified Data.Map.Strict                  as M
import           Data.Maybe
import qualified Data.Sequence                    as SQ
import qualified Data.Set                         as S

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
    readChilds = AC.takeWhile (not . isLetter) *> AC.takeWhile isLetter `sepBy` string ", "
    readTree = do
      ident <- readIdent
      char ' '
      w <- readWeight
      childs <- readChilds
      return $ FlatTree ident w childs
    tree = parseOnly readTree

data Instruction = Instruction { register :: BS.ByteString, method :: Method, methodValue :: Int, condition :: Condition } deriving Show
data Method = Inc | Dec deriving (Show)
data Condition = Condition { targetReg :: BS.ByteString, operator :: Operator, checkValue :: Int } deriving Show
data Operator = G | L | GE | LE | E | NE deriving Show

day8 :: Level
day8 content = return $ interpret 0 0 M.empty ((fromRight [] . mapM (parseOnly readInstruction) . nonEmptyLines) content)
  where
    readValue = signed decimal
    readRegister = AC.takeWhile isLetter
    readMethod =
      ("inc" >> pure Inc) <|>
      ("dec" >> pure Dec)
    readOperator =
      (">=" >> pure GE) <|>
      ("<=" >> pure LE) <|>
      ("==" >> pure E) <|>
      ("!=" >> pure NE) <|>
      (">" >> pure G) <|>
      ("<" >> pure L)
    readCondition = Condition <$> ("if" *> skipSpace *> readRegister) <* skipSpace <*> readOperator <* skipSpace <*> readValue
    readInstruction = Instruction <$> readRegister <* skipSpace <*> readMethod <* skipSpace <*> readValue <* skipSpace <*> readCondition
    interpret !i !m !regs !stack
      | i >= length stack = (maxValue, m)
      | otherwise = let (Instruction ir im imv ic) = stack !! i
                        f = case im of
                          Inc -> (+)
                          Dec -> (-)
                        nextValue x = Just $ f (fromMaybe 0 x) imv
                        nextIndex = i + 1
                        nextMax = max m maxValue
                    in
                      if validCondition ic then
                        interpret nextIndex nextMax (M.alter nextValue ir regs) stack
                      else
                        interpret nextIndex nextMax regs stack
      where
        maxi xs
          | null xs   = 0
          | otherwise = maximum xs
        maxValue = maxi . M.elems $ regs
        validCondition (Condition reg op checkValue) = (opf op) rv checkValue
            where
              rv = M.findWithDefault 0 reg regs
              opf op = case op of
                G  -> (>)
                L  -> (<)
                GE -> (>=)
                LE -> (<=)
                E  -> (==)
                NE -> (/=)

days :: [(String, Level)]
days = [("day8", day8)]

run :: IO ()
run = join $ mapM_ putStrLn <$> mapM (\(n, f) -> ((++) (n ++ " -> ") . show <$>) . f =<< contentOf (n ++ ".txt")) days
