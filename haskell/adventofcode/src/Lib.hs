-- Lib.hs ---

-- Copyright (C) 2017 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.


{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString.Char8 as AC
import           Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Internal         as BSI
import           Data.Char
import           Data.Either
import           Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Maybe
import qualified Data.Sequence                    as SQ
import qualified Data.Set                         as S
import           Data.Word
import           Test.Hspec

type Level = Input -> IO Output
type Input = BS.ByteString
type Output = BS.ByteString

contentOf :: String
          -> IO Input
contentOf = BS.readFile . ("./files/" ++)

readInt :: String -> Int
readInt = read

nonEmpty :: [Input] -> [Input]
nonEmpty = filter (/= BS.empty)

nonEmptyLines :: Input -> [Input]
nonEmptyLines = nonEmpty . BS.lines

showParts :: (Show a, Show b) => a -> b -> IO BS.ByteString
showParts a b = return $ BS.pack ("a=" ++ show a ++ ", b=" ++ show b)

day4 :: Level
day4 content = let part1 = validLength id
                   part2 = validLength $ map BS.sort
               in
                 showParts part1 part2
  where
    validLength f = length . filter (isValid f) . nonEmptyLines $ content
    isValid projection line = length original == (S.size . uniques) original
      where
        words = BS.split ' '
        original = projection . words $ line
        uniques = S.fromList

day5 :: Level
day5 content = let part1 = run id 0 0 maze
                   part2 = run  (\x -> if x >= 3 then -1 else 1) 0 0 maze
               in
                 showParts part1 part2
  where
    maze = SQ.fromList . map (readInt . BS.unpack) . nonEmptyLines $ content
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
day6 content = return ""

data FlatTree = FlatTree { identifier :: BS.ByteString, weight :: Int, childrens :: [BS.ByteString] } deriving (Show)

instance Eq FlatTree where
  (==) a b = identifier a == identifier b

day7 :: Level
day7 content = let x = mapM tree . nonEmptyLines $ content
               in case x of
                    Right trees -> do
                      let
                        identifiers = map identifier trees
                        stupids = join $ map childrens trees
                        Just x = find (not . (flip elem) stupids) identifiers
                      showParts 0 0
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
day8 content = let (a, b) = interpret 0 0 M.empty ((fromRight [] . mapM (parseOnly readInstruction) . nonEmptyLines) content)
               in
                 showParts a b
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

data Block = Block { score :: Int, garbage :: Int }

day9 :: Level
day9 content = let block = fromRight (Block (-1) 0) run
               in
                 showParts (score block) (garbage block)
  where
    readBlock !block@(Block !sc !cs) =
      ("{" >> continueWith combineBlocks (readBlock innerBlock)) <|>
      ("<" >> continueWith combineGarbage (readGarbage block)) <|>
      ("}" >> pure block) <|>
      (AC.anyChar >> readBlock block)
      where
        innerBlock = Block (sc + 1) cs
        combineBlocks (Block a b) (Block a' b') = Block (a + a') (b + b')
        combineGarbage (Block a b) (Block a' b') = Block a' (b + b')
        continueWith f p = f <$> p <*> readBlock block
    readGarbage !block@(Block !sc !cs) =
      ("!" >> AC.anyChar *> readGarbage block) <|>
      (">" >> pure block) <|>
      (AC.anyChar >> readGarbage nextBlock)
      where
        nextBlock = Block sc (cs + 1)
    run = parseOnly (readBlock (Block 1 0)) $ content

day10 :: Level
day10 content = let lengths = map (readInt . BS.unpack) . BS.split ',' $ content
                    (_, _, finalBuffer) = foldl' run (0, 0, [0..255]) lengths
                    part1 = product . L.take 2 $ finalBuffer
                in
                  showParts part1 0
  where
     run (position, skip, buffer) reverseLength   = ((position + reverseLength + skip) `mod` bufferLength, skip + 1, nextBuffer)
       where
         bufferLength = length buffer
         (rightPart, leftPart) = splitAt position . cycle $ buffer
         (target, rest) = splitAt reverseLength leftPart
         reconstructed = rightPart ++ (reverse target) ++ rest
         nextCycleIndex = if position + reverseLength > bufferLength then (position + reverseLength) `mod` bufferLength else 0
         shift = L.take nextCycleIndex . drop bufferLength $ reconstructed
         nextBuffer = L.take bufferLength (shift ++ drop nextCycleIndex reconstructed ++ reconstructed)

day11 :: Level
day11 content = let directions = join . map (nonEmpty . BS.split ',') $ nonEmptyLines content
                    (x, y, z, m) = foldl' run (0, 0, 0, 0) directions
                    distance = dist [x, y, z]
                in
                  showParts distance m
  where
    dist = (/ 2) . fromIntegral . sum . map abs
    run (x, y, z, m) d = (x', y', z', m')
      where
        (x', y', z') = case d of
                         "n"  -> (x, y + 1, z - 1)
                         "ne" ->  (x + 1, y, z - 1)
                         "nw" ->  (x - 1, y + 1, z)
                         "s"  -> (x, y - 1, z + 1)
                         "sw" ->  (x - 1, y, z + 1)
                         "se" ->  (x + 1, y - 1, z)
        m' = max m (dist [x', y', z'])

day12 :: Level
day12 content = let a = S.size . connections S.empty $ 0
                    b = S.size . S.fromList . map (connections S.empty) . M.keys $ dict
                in
                  showParts a b
  where
    connection = (,) <$> (decimal <* " <-> ") <*> decimal `sepBy` ", "
    dict = M.fromList . fromRight [] . mapM (parseOnly connection) . nonEmptyLines $ content
    connections !visited x = let childs = S.fromList . fromMaybe [] $ M.lookup x dict
                                 visited' = S.insert x . S.union childs $ visited
                             in
                              S.foldl' S.union (S.singleton x) . S.map (connections visited') . S.filter (flip S.notMember visited) $ childs

day13 :: Level
day13 content = let delayedSeverity delay = map (severity delay) $ layers
                    a = sum . map snd $ delayedSeverity 0
                    reduce = foldl' (\(ba, va) (bb, vb) -> (ba || bb, va + vb)) (False, 0)
                    b = map ((,) <$> reduce . delayedSeverity <*> id) [0..]
                in
                  showParts a (L.take 1 . filter  (not . fst . fst) $ b)
  where
    layer = (,) <$> (decimal <* ": ") <*> decimal
    layers = fromRight [] . mapM (parseOnly layer) . nonEmptyLines $ content
    severity t (d, r) = (caught, if caught then d * r else 0)
      where
        round = 2 * r - 2
        time = t + d
        caught = time `mod` round == 0

day14 :: Level
day14 content = return . BS.pack . show $ rows
  where
    rows = map ((++) (BS.unpack content ++ "-") . show) [0..127]

day15 :: Level
day15 content = return "Not yet hehe !!"


data Dance = Spin Int | Exchange Int Int | Partner Char Char deriving Show

day16 :: Level
day16 content = showParts (foldl' disco ['a'..'p'] $ dances . BS.split ',' $ content) 0
  where
    dances = fromRight [] . mapM (parseOnly dance)
    dance =
      ("s" >> Spin <$> decimal) <|>
      ("x" >> Exchange <$> (decimal <* AC.anyChar) <*> decimal) <|>
      ("p" >> Partner <$> (AC.anyChar <* AC.anyChar) <*> AC.anyChar)
    disco !game !(Spin nb) = (snd chunks) ++ (fst chunks)
      where
        gameLength = length game
        chunks = splitAt (gameLength - nb) game
    disco !game !(Exchange i j) = swapElts i j game
      where
        swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
          where
            get k x | k == i = ls !! j
                    | k == j = ls !! i
                    | otherwise = x
    disco !game !(Partner a b) = disco game (Exchange i j)
      where
        i = indexOf a
        j = indexOf b
        indexOf = fromMaybe (-1) . (flip elemIndex $ game)

days :: [(String, Level)]
days = [("day16", day16)]

run :: IO ()
run = join $ mapM_ putStrLn <$> mapM (\(n, f) -> ((++) (n ++ " -> ") . BS.unpack <$>) . f =<< contentOf (n ++ ".txt")) days
