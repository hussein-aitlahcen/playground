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

module Lib where

import           Data.Bifunctor
import           Data.Char
import           Data.List

upperCase :: String -> String
upperCase = map toUpper

httpFormat :: [(String, String)]
           -> [(String, String) -> (String, String)]
           -> [(String, String)]
httpFormat elements transformations = map transMonoid elements
  where
    transMonoid = foldl' (.) id transformations

entry :: IO ()
entry = print $ httpFormat elements transformations
  where
    elements = [("User-Agent", "Mozilla")]
    transformations = [bimap upperCase id, bimap id upperCase]
