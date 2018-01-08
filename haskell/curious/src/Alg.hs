-- Alg.hs ---

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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE BangPatterns #-}

module Alg where

import           Data.Bifunctor
import           Data.Functor.Base
import           Data.Functor.Foldable
import           Data.List
import           Data.Vector           as V
import           Text.Show.Deriving

{-
    We want to transform the raw data into a binary tree, then compute the maximum triangle path sum
    as explained here https://rosettacode.org/wiki/Maximum_triangle_path_sum.

    Using recursion scheme, we define an algebra and its coalgebra, composing them together
    allow us to compute what we want.
-}

data BinTreeF a b = NodeF a b b
                  | LeafF
                  deriving Functor

deriveShow1 ''BinTreeF

type Input      = Vector (Vector Int)
type MaxSumPath = Int

coalgebra :: (Int, Int, Input) -> BinTreeF Int (Int, Int, Input)
coalgebra (i, j, v)
  | V.length v == i = LeafF
  | otherwise = NodeF (c ! j) (i', lj, v) (i', rj, v)
  where
    c = v ! i
    i' = i + 1
    lj = max (j - 1) 0
    rj = j + 1

algebra :: BinTreeF Int MaxSumPath -> MaxSumPath
algebra LeafF         = 0
algebra (NodeF x l r) = x + max l r

maxPathSum = hylo algebra coalgebra . ((,,) 0 0)
