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

f, h:: Pipe Int Int
f = arr $ (* 3) . (+ 1)
h = f &&& f >>> (arr . uncurry) (+)

entry :: IO ()
entry = print $ runF h 1
