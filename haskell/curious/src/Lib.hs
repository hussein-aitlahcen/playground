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


{-# LANGUAGE Arrows #-}

module Lib where

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Prelude             hiding (id, (.))

newtype Pipe a b = Pipe { runF :: (a -> b) }

instance Arrow Pipe where
  arr f = Pipe f
  first (Pipe f) = Pipe (f *** id)
  second (Pipe f) = Pipe (id *** f)

instance Category Pipe where
  (Pipe g) . (Pipe f) = Pipe (g . f)
  id = arr id

f, h :: Pipe Int Int
f = arr $ (* 3) . (+ 1)
h = f &&& f >>> (arr . uncurry) (+)

entry :: IO ()
entry = print $ runF h 1
