module Palytte.Control.Monad where

import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Control.Monad (filterM)

-- Given a monadic filtering function and a list of values, return the first
-- value passing the filter (or None)
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM filter xs = filterM filter xs <&> listToMaybe
