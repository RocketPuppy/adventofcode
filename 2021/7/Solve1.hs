module Solve1 (solve1) where

import Types (Crab(..), fuel, median)

solve1 crabs = fuel crabs (median crabs)
