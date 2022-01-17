module Solve1 (solve1, intersections) where

import Types (LineSegment(..), Point(..), getX1, getX2, getY1, getY2, horizontal, vertical)
import Data.List (group, sort)

solve1 = intersections . filter (\s -> horizontal s || vertical s)

intersections :: [LineSegment] -> Int
intersections = length . filter (\s -> length s > 1) . group . sort . toGridPoints

toGridPoints :: [LineSegment] -> [Point]
toGridPoints = concatMap toGridPoints'

-- Iterate to fixed point
iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f x
    | x == f x = [x]
    | otherwise = x:(iterateFix f (f x))

toGridPoints' :: LineSegment -> [Point]
toGridPoints' = fmap get1 . iterateFix bumpSegment

-- Will converge on a line segment that is a point equal to point 2
bumpSegment :: LineSegment -> LineSegment
bumpSegment seg
    | getX1 seg < getX2 seg && horizontal seg = LineSegment (Point (succ $ getX1 seg) (getY1 seg)) (get2 seg)
    | getX1 seg > getX2 seg && horizontal seg = LineSegment (Point (pred $ getX1 seg) (getY1 seg)) (get2 seg)
    | vertical seg && getY1 seg < getY2 seg = LineSegment (Point (getX1 seg) (succ $ getY1 seg)) (get2 seg)
    | vertical seg && getY1 seg > getY2 seg = LineSegment (Point (getX1 seg) (pred $ getY1 seg)) (get2 seg)
    | getX1 seg < getX2 seg && getY1 seg < getY2 seg = LineSegment (Point (succ $ getX1 seg) (succ $ getY1 seg)) (get2 seg)
    | getX1 seg < getX2 seg && getY1 seg > getY2 seg = LineSegment (Point (succ $ getX1 seg) (pred $ getY1 seg)) (get2 seg)
    | getX1 seg > getX2 seg && getY1 seg < getY2 seg = LineSegment (Point (pred $ getX1 seg) (succ $ getY1 seg)) (get2 seg)
    | getX1 seg > getX2 seg && getY1 seg > getY2 seg = LineSegment (Point (pred $ getX1 seg) (pred $ getY1 seg)) (get2 seg)
    | otherwise = seg
