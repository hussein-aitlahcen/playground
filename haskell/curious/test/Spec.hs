-- Spec.hs ---

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


import           Data.Bifunctor
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "formating http headers" $ do
    it "should be associative in its transformation composition (i.e. full transformation monoid)" $ do
      let
        headers = [("User-Agent", "Mozilla"),
                   ("Dumb", "Unicorn")]
        transformations = [bimap (const "blind") id,
                           bimap id (const "ignorant")]
        expected = [("blind", "ignorant"),
                  ("blind", "ignorant")]
      httpFormat headers transformations `shouldBe` expected
      httpFormat headers (reverse transformations) `shouldBe` expected
