-- Algebra.hs ---

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

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Alg where

import           Data.Bifunctor
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List

-- Using F-algebra to resolve https://rosettacode.org/wiki/Maximum_triangle_path_sum

data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a deriving Show

makeBaseFunctor ''BinTree

type Path = [Int]
type MaxSumPath = (Int, Path)

algebra :: BinTreeF Int MaxSumPath -> MaxSumPath
algebra (LeafF _)            = (0, [])
algebra (NodeF x left right) = bimap ((+) x) ((:) x) (f left right)
  where
    f a@(v, _) b@(w, _)
      | v > w = a
      | v < w = b
      | v == w = a

tree :: BinTree Int
tree = Node 8
    (Node 3
      (Node 2
       (Node 10
         (Leaf 0)
         (Leaf 0))
        (Leaf 0))
      (Node 10
        (Leaf 0)
        (Leaf 0)))
    (Node 1
      (Node 10
       (Node 20
        (Leaf 0)
        (Leaf 0))
       (Leaf 0))
      (Leaf 0))

algTest = cata algebra tree
