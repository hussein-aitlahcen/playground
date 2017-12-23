{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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

type Id = Int
type Friction = Float
type Rotation = Float
type Position = (Float, Float)
type Velocity = (Float, Float)
type Time = Float

class Accelerable a where
  accelerate :: Time -> Velocity -> a -> a

instance Accelerable Origin where
  accelerate dt (vx, vy) (Origin id (x, y) o) = Origin id (x', y') o
    where
      x' = x + vx * dt
      y' = y + vy * dt

class Frictionable a where
  friction :: Friction -> a -> a

instance Frictionable WithVelocity where
  friction ratio (WithVelocity (vx, vy) o) = WithVelocity (vx', vy') o
    where
      r = 1 - ratio
      vx' = vx * r
      vy' = vy * r

class Updatable a where
  update :: Time -> a -> a

instance Updatable GameObject where
  update dt (GameObject o) = GameObject $ update dt o

instance Updatable Origin where
  update dt (Origin i p c) = (Origin i p (map (update dt) c))

instance Updatable WithFriction where
  update dt (WithFriction r o) = WithFriction r (update dt . friction r $ o)

instance Updatable WithVelocity where
  update dt (WithVelocity v o) = WithVelocity v (update dt . accelerate dt v $ o)

instance Show WithVelocity where
  show (WithVelocity v o) = "WithVelocity { velocity=" ++ show v ++  ", origin=" ++ show o

instance Show WithFriction where
  show (WithFriction r o) = "Friction { ratio=" ++ show r ++ ", origin=" ++ show o

instance Show Origin where
  show (Origin id p c) = "Origin { id=" ++ show id ++ ", pos=" ++ show p ++ ", children=" ++ show c

instance Show GameObject where
  show (GameObject o) = show o

data GameObject = forall a. (Show a, Updatable a) => GameObject a
data Origin = forall a. (Show a, Updatable a) => Origin { id :: Id, position :: Position, childen :: [a] }
data WithVelocity = forall a. (Show a, Updatable a, Accelerable a) => WithVelocity Velocity a
data WithFriction = forall a. (Show a, Updatable a, Frictionable a) => WithFriction Friction a

ship = WithFriction 0.05 $ WithVelocity (5, 5) $ Origin 0 (0, 0) ([a, b])
  where
    a = GameObject $ WithVelocity (10, 10) $ Origin 0 (0, 0) ([] :: [Origin])
    b = GameObject $ Origin 0 (0, 0) ([] :: [Origin])

play :: IO ()
play = print $ update 1 ship
