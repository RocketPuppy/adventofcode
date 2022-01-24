{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.List (sort)
import Debug.Trace

newtype Crab = Crab { position :: Int }
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)

median :: [Crab] -> Crab
median crabs = head . drop (length crabs `div` 2) . sort $ crabs

fuel :: [Crab] -> Crab -> Int
fuel crabs target = foldr (\c t -> t + abs ((position c) - (position target))) 0 crabs

-- A binary search to find the midpoint by searching for a slope of zero
fuel2 :: [Crab] -> Crab -> Maybe Int -> Int
fuel2 crabs target Nothing = fuel2 crabs (succ target) (Just (costTotal target crabs))
fuel2 crabs target (Just prevCost)
    | cost > prevCost = prevCost
    | otherwise = fuel2 crabs (succ target) (Just cost)
    where
        cost = costTotal target crabs

costTotal target = sum . fmap (cost target)

newTarget :: [Crab] -> Crab -> Crab
newTarget crabs target
    | target < maximum crabs = target + (((maximum crabs) - target) `div` 2)
    | otherwise = target - ((target - (minimum crabs)) `div` 2)

-- 1 + 2 + 3 + 4 + n ... = n(n+1)/2
-- (n-m)((n-m)+1)/2 n is crab, m is center point
-- (n^2 - 2nm + m^2 +n - m)/2
-- cost m n = ((n*n) - (2*n*m) + (m*m) + n - m)
-- Target (m) comes first to make the code nicer
cost (Crab m) (Crab n) = ((steps n m)*((steps n m) + 1)) `div` 2

steps n m = abs (n - m)
