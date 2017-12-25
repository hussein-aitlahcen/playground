-- World.hs ---

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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Object where

import           Common
import           Control.Lens

data Player = Player { _position :: Position, _velocity :: Velocity }
data World = World { _iteration :: Int, _player :: Player }

class HasIteration s a | s -> a where
  iteration :: Lens' s a

class HasPlayer s a | s -> a where
  player :: Lens' s a

class HasPosition s a | s -> a where
  position :: Lens' s a

class HasVelocity s a | s -> a where
  velocity :: Lens' s a

instance HasPosition Player Position where
  position f (Player p v) = fmap (flip Player $ v) (f p)
  {-# INLINE position #-}

instance HasVelocity Player Velocity where
  velocity f (Player p v) = fmap (Player p) (f v)
  {-# INLINE velocity #-}

instance HasIteration World Int where
  iteration f (World i p) = fmap (flip World $ p) (f i)
  {-# INLINE iteration #-}

instance HasPlayer World Player where
  player f (World i p) = fmap (World i) (f p)
  {-# INLINE player #-}
