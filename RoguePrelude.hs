module RoguePrelude (module X) where

import BasePrelude as X hiding (map, lookup, (&), index, left, right)
import Data.Array as X hiding (index)
