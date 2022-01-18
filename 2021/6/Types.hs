module Types where

data Fish = Fish { daysUntilBirth :: Int }
    deriving (Show, Eq)

data Days = Days
    { day0 :: Int
    , day1 :: Int
    , day2 :: Int
    , day3 :: Int
    , day4 :: Int
    , day5 :: Int
    , day6 :: Int
    , day7 :: Int
    , day8 :: Int
    }
    deriving (Show, Eq)

countFishes :: Days -> Int
countFishes d = day0 d + day1 d + day2 d + day3 d + day4 d + day5 d + day6 d + day7 d + day8 d

-- For debugging days
printFishes :: Days -> IO ()
printFishes days = do
    print $ "0: " ++ show (day0 days)
    print $ "1: " ++ show (day1 days)
    print $ "2: " ++ show (day2 days)
    print $ "3: " ++ show (day3 days)
    print $ "4: " ++ show (day4 days)
    print $ "5: " ++ show (day5 days)
    print $ "6: " ++ show (day6 days)
    print $ "7: " ++ show (day7 days)
    print $ "8: " ++ show (day8 days)

emptyDays :: Days
emptyDays = Days 0 0 0 0 0 0 0 0 0

addFish :: Fish -> Days -> Days
addFish (Fish 0) d = d { day0 = succ (day0 d) }
addFish (Fish 1) d = d { day1 = succ (day1 d) }
addFish (Fish 2) d = d { day2 = succ (day2 d) }
addFish (Fish 3) d = d { day3 = succ (day3 d) }
addFish (Fish 4) d = d { day4 = succ (day4 d) }
addFish (Fish 5) d = d { day5 = succ (day5 d) }
addFish (Fish 6) d = d { day6 = succ (day6 d) }
addFish (Fish 7) d = d { day7 = succ (day7 d) }
addFish (Fish 8) d = d { day8 = succ (day8 d) }

daysFromFishes :: [Fish] -> Days
daysFromFishes fs = foldr addFish emptyDays fs

age :: Days -> Days
age d = d
    { day0 = day1 d
    , day1 = day2 d
    , day2 = day3 d
    , day3 = day4 d
    , day4 = day5 d
    , day5 = day6 d
    , day6 = day7 d + day0 d
    , day7 = day8 d
    , day8 = day0 d
    }
