module Solve2 (solve2) where

import Types (Crab(..), fuel2, newTarget)
import Data.List (sort)

solve2 crabs = fuel2 (sort crabs) (Crab 0) Nothing
