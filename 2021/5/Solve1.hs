module Solve1 (solve1, intersections) where

import Prelude hiding (takeWhile)
import Types (LineSegment(..), Point(..), getX1, getX2, getY1, getY2, horizontal, vertical)
import Data.List (group, sort, unfoldr)

solve1 = intersections . filter (\s -> horizontal s || vertical s)

intersections :: [LineSegment] -> Int
intersections = length . filter (\s -> length s > 1) . group . sort . toGridPoints

toGridPoints :: [LineSegment] -> [Point]
toGridPoints = concatMap $ fmap get1 . iterateFix bumpSegment

-- Iterate to fixed point
iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f x = takeWhile (/=) . iterate f $ x

takeWhile :: (a -> a -> Bool) -> [a] -> [a]
takeWhile _ (l:[]) = [l]
takeWhile f (l:l':ls)
    | f l l' = l:(takeWhile f (l':ls))
    | otherwise = [l]

-- Will converge on a line segment that is a point equal to point 2
bumpSegment :: LineSegment -> LineSegment
bumpSegment seg
    | getX1 seg < getX2 seg && horizontal seg        = LineSegment (Point (succ $ getX1 seg)        (getY1 seg)) (get2 seg)
    | getX1 seg > getX2 seg && horizontal seg        = LineSegment (Point (pred $ getX1 seg)        (getY1 seg)) (get2 seg)
    | vertical seg          && getY1 seg < getY2 seg = LineSegment (Point        (getX1 seg) (succ $ getY1 seg)) (get2 seg)
    | vertical seg          && getY1 seg > getY2 seg = LineSegment (Point        (getX1 seg) (pred $ getY1 seg)) (get2 seg)
    | getX1 seg < getX2 seg && getY1 seg < getY2 seg = LineSegment (Point (succ $ getX1 seg) (succ $ getY1 seg)) (get2 seg)
    | getX1 seg < getX2 seg && getY1 seg > getY2 seg = LineSegment (Point (succ $ getX1 seg) (pred $ getY1 seg)) (get2 seg)
    | getX1 seg > getX2 seg && getY1 seg < getY2 seg = LineSegment (Point (pred $ getX1 seg) (succ $ getY1 seg)) (get2 seg)
    | getX1 seg > getX2 seg && getY1 seg > getY2 seg = LineSegment (Point (pred $ getX1 seg) (pred $ getY1 seg)) (get2 seg)
    | otherwise = seg
