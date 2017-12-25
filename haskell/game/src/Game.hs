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

module Game where

import           Common
import           Control.Lens
import           Control.Lens.Operators
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Interface.Pure.Game
import           Object

window = InWindow "Game" (600, 600) (600, 100)

initialPlayer = Player (0, 0) (0, 0)
initialWorld = World 0 initialPlayer

worldPicture :: World -> Picture
worldPicture w = color white . (uncurry translate . view (player . position)) w $ thickCircle 10 20

worldTransform :: Event -> World -> World
worldTransform (EventKey (SpecialKey sk) ks _ _) w@(World _ p@(Player _ (vx, vy)))
  | sk == KeyLeft && ks == Down = vel _1 (-1)
  | sk == KeyLeft && ks == Up = vel _1 0
  | sk == KeyRight && ks == Down = vel _1 1
  | sk == KeyRight && ks == Up = vel _1 0
  | sk == KeyUp && ks == Down = vel _2 1
  | sk == KeyUp && ks == Up = vel _2 0
  | sk == KeyDown && ks == Down = vel _2 (-1)
  | sk == KeyDown && ks == Up = vel _2 0
  where vel f c = w & player . velocity . f .~ c
        nle 0 0 = (0, 0)
        nle x y = normalizeV (x, y)
worldTransform e w = w

worldIterate :: Float -> World -> World
worldIterate dt w = w &
                    (iteration %~ (+ 1))
                    . (player . position %~ (stepMove |+|))
  where stepMove = mulSV 400
                   . mulSV dt
                   . view (player . velocity)
                   $ w

runGame :: IO ()
runGame = play window black 60 initialWorld worldPicture worldTransform worldIterate
