module Solve1 (solve1) where

import Types (age, countFishes)

solve1 = countFishes . head . drop 80 . iterate age
