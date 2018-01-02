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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

fps :: Int
fps = 60

speed :: Speed
speed = 400

window :: Display
window = InWindow "Game" (600, 600) (600, 100)

initialPlayer :: Player
initialPlayer = Player (0, 0) (0, 0)

initialWorld :: World
initialWorld = World 0 initialPlayer

worldPicture :: World -> Picture
worldPicture w = color white . (uncurry translate . view (player . position)) w $ thickCircle 10 20

worldTransform :: (HasVelocity a Vector, HasPlayer p a) => Event -> p -> p
worldTransform (EventKey (SpecialKey sk) ks _ _) w
  | sk == KeyLeft && ks == Down = updateVelocity first (-1)
  | sk == KeyLeft && ks == Up = updateVelocity first 0
  | sk == KeyRight && ks == Down = updateVelocity first 1
  | sk == KeyRight && ks == Up = updateVelocity first 0
  | sk == KeyUp && ks == Down = updateVelocity second 1
  | sk == KeyUp && ks == Up = updateVelocity second 0
  | sk == KeyDown && ks == Down = updateVelocity second (-1)
  | sk == KeyDown && ks == Up = updateVelocity second 0
  where
    updateVelocity comp val = w & player . velocity %~ safeNormV . comp (const val)
worldTransform e w = w

translateV :: Time -> Speed -> Vector -> Vector
translateV dt s = mulSV (s * dt)

translateObj :: (HasPosition a Point, HasVelocity a Velocity) => Time -> a -> a
translateObj dt p = p & position %~ ((|+|) (translateV dt speed $ view velocity p))

timestepObj :: (HasVelocity a Velocity, HasPosition a Point, HasPlayer b a, HasIteration b Int) => Time -> b -> b
timestepObj dt w = w & (iteration %~ (+ 1)) . (player %~ translateObj dt)

runGame :: IO ()
runGame = play window black fps initialWorld worldPicture worldTransform timestepObj
