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

import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Interface.Pure.Game

type Velocity = Vector
type Position = Point

(|+|) :: Point -> Point -> Point
(|+|) (ax, ay) (bx, by) = (ax + bx, ay + by)

data Player = Player { position :: Position, velocity :: Velocity }
data World = World { iteration :: Int, player :: Player }

draw :: World -> Picture
draw w = color white . (uncurry translate . position . player) w $ thickCircle 10 20

input :: Event -> World -> World
input (EventKey (SpecialKey sk) ks _ _) w@(World _ p@(Player _ (vx, vy)))
  | sk == KeyLeft && ks == Down = applyVel (-1) vy
  | sk == KeyLeft && ks == Up = applyVel 0 vy
  | sk == KeyRight && ks == Down = applyVel 1 vy
  | sk == KeyRight && ks == Up = applyVel 0 vy
  | sk == KeyUp && ks == Down = applyVel vx 1
  | sk == KeyUp && ks == Up = applyVel vx 0
  | sk == KeyDown && ks == Down = applyVel vx (-1)
  | sk == KeyDown && ks == Up = applyVel vx 0
  where
    applyVel x y = w { player = p { velocity = normalized x y } }
    normalized 0 0 = (0, 0)
    normalized x y = normalizeV (x, y)
input e w = w

step :: Float -> World -> World
step dt w@(World i (Player p v)) = World (i + 1) (Player (stepMove |+| p) v)
  where
    stepMove = mulSV 400 . mulSV dt $ v

runGame :: IO ()
runGame = play (InWindow "Game" (600, 600) (600, 100)) black 60 (World 0 (Player (0, 0) (0, 0))) draw input step
