-- Game.hs ---

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

type Position = (Float, Float)
type Velocity = (Float, Float)

data Entity = Origin { id :: Int, children :: [Entity] }
            | WithPosition { position :: Position , origin :: Entity }
            | WithRotation { angle :: Int, origin :: Entity }

            -- Recursive induction are not yet implemented in Haskell, we can't constraint origin to be of type WithPosition
            -- But this will be implemented in next version of GHC :D.
            | WithVelocity { velocity :: Velocity, origin :: Entity }
            deriving Show

update :: Float -> Entity -> Entity
update dt (Origin i c) = Origin i (map (update dt) c)
update dt (WithPosition p o) = WithPosition p (update dt o)
update dt (WithRotation a o) = WithRotation a (update dt o)
update dt (WithVelocity (vx, vy) (WithPosition (x, y) o)) = WithVelocity (vx, vy) (update dt (WithPosition (x', y') o))
  where
    x' = x + vx * dt
    y' = y + vy * dt

ship  :: Entity
ship  = WithVelocity (5, 5) $ WithPosition (0, 0) $ WithRotation 0 $ Origin 0 []

play :: IO ()
play = print $ update 1 ship
