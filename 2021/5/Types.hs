module Types where

data Point = Point { getX :: Int, getY :: Int }
    deriving (Show, Eq, Ord)

data LineSegment = LineSegment { get1 :: Point, get2 :: Point }
    deriving (Show, Eq)

getX1 = getX . get1
getX2 = getX . get2
getY1 = getY . get1
getY2 = getY . get2

horizontal seg = getY1 seg == getY2 seg
vertical seg = getX1 seg == getX2 seg

isPoint seg = getX1 seg == getX2 seg && getY1 seg == getY2 seg
