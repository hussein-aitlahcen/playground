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

{-# LANGUAGE FlexibleContexts #-}

module Game where

import           Common
import           Control.Lens
import           Control.Lens.Operators
import           Data.Bifunctor
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Interface.Pure.Game
import           Object

window :: Display
window = InWindow "Game" (600, 600) (600, 100)

speed :: Float
speed = 400

initialPlayer :: Player
initialPlayer = Player (0, 0) (0, 0)

initialWorld :: World
initialWorld = World 0 initialPlayer

worldPicture :: World -> Picture
worldPicture w = color white . (uncurry translate . view (player . position)) w $ thickCircle 10 20

transformObj :: (HasVelocity s Vector) => ((Float -> Float) -> Vector -> Vector) -> Float -> s -> s
transformObj f v = velocity %~ safeNormV . f (const v)

worldTransform :: (HasVelocity a Vector, HasPlayer p a) => Event -> p -> p
worldTransform (EventKey (SpecialKey sk) ks _ _) w
  | sk == KeyLeft && ks == Down = vel first (-1)
  | sk == KeyLeft && ks == Up = vel first 0
  | sk == KeyRight && ks == Down = vel first 1
  | sk == KeyRight && ks == Up = vel first 0
  | sk == KeyUp && ks == Down = vel second 1
  | sk == KeyUp && ks == Up = vel second 0
  | sk == KeyDown && ks == Down = vel second (-1)
  | sk == KeyDown && ks == Up = vel second 0
  where vel f v = w & player %~ transformObj f v
worldTransform e w = w

translateV :: Float -> Float -> Vector -> Vector
translateV dt s = mulSV (s * dt)

translateObj :: (HasPosition a Point, HasVelocity a Velocity) => Float -> a -> a
translateObj dt p = p & position %~ ((|+|) (translateV dt speed $ view velocity p))

timestepObj :: (HasVelocity a2 Velocity, HasPosition a2 Point, HasPlayer b a2, Num a1, HasIteration b a1) => Float -> b -> b
timestepObj dt w = w & (iteration %~ (+ 1)) . (player %~ translateObj dt)

runGame :: IO ()
runGame = play window black 60 initialWorld worldPicture worldTransform timestepObj
