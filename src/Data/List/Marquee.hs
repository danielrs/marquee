module Data.List.Marquee where

import Data.Maybe (fromMaybe)

lookupOr :: (Eq key) => a -> key -> [(key, a)] -> a
lookupOr backup key = fromMaybe backup . lookup key

