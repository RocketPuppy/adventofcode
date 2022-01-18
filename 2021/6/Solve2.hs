module Solve2 (solve2) where

import Types (age, countFishes)

solve2 = countFishes . head . drop 256 . iterate age
