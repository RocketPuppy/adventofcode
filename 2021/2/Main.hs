{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Grammar.Layout ( resolveLayout )
import Grammar.Par (pListCommand, myLexer)
import Grammar.Abs
import Grammar.ErrM

-- Part 1 is easily represented as 2-D vectors. A direction command maps to a
-- change in horizontal position or depth.
data Vector = Vector
    { getH :: Integer
    , getDepth :: Integer
    }

hVector h = Vector h 0
dVector d = Vector 0 d

-- ctov coverts commands to their equivalent vector.
ctov :: Command -> Vector
ctov (Command Up (Distance d)) = dVector (negate d)
ctov (Command Down (Distance d)) = dVector d
ctov (Command Forward (Distance d)) = hVector d

-- A semigroup is the name for a type paired with an operation for combining
-- elements of that type. In Haskell this is defined as a polymorphic operator
-- "<>". Any type that implements Semigroup will work with this operator. It is
-- important that the operator be associative, a property that will become
-- obviously important for part 2. Vector addition is already associative, so
-- it's just written down here.
-- Example: Integers paired with addition form a semigroup.
instance Semigroup (Vector) where
    (Vector h d) <> (Vector h' d') = Vector (h + h') (d + d')

-- A monoid is an extension to a semigroup that provides an identity element.
-- This allows for the definition of generic functions like mconcat that take a
-- possibly empty list of monoidal elements and combine them into one. The
-- identity element for vectors is simply the zero vector.
-- Example; The addition semigroup over Integers forms a monoid with 0.
instance Monoid (Vector) where
    mempty = Vector 0 0

-- Solving part 1 then becomes easy. First I map all the commands to vectors
-- "fmap ctov". Then they are combined with mconcat. Third the height and depth
-- are multiplied. And last the result is converted to a string with show.
solve1 :: [Command] -> String
solve1 commands = show . (\v -> getH v * getDepth v) . mconcat . fmap ctov $ commands

-- Part 2 changes things up by imposing non-associativity on the commands. That
-- is, evaluating forward now requires the depth from the previous step. This
-- means I get a different answer if I evaluate left-to-right vs.
-- right-to-left. So I can't define a semigroup that doesn't have surprising
-- behavior. Instead I will define a fold from the left over the list of
-- commands (this is also a catamorphism), resulting in a submarine model.
data Submarine = Submarine
    { getHSub :: Integer
    , getDepthSub :: Integer
    , getAimSub :: Integer
    }

zeroSub = Submarine 0 0 0

addAim i (Submarine h d a) = Submarine h d (a+i)
addDepth i (Submarine h d a) = Submarine h (d+i) a
addH i (Submarine h d a) = Submarine (h+i) d a

-- The fold pattern matches on the command and does what the problem describes.
foldCommand :: Command -> Submarine -> Submarine
foldCommand (Command Up (Distance d)) s = addAim (negate d) s
foldCommand (Command Down (Distance d)) s = addAim d s
foldCommand (Command Forward (Distance d)) s = addH d . addDepth (d * getAimSub s) $ s

-- Solving is just a matter of plugging in the fold, multiplying the resulting
-- values, and coverting it to a string. The left fold expects its arguments in
-- a different order so flip is used to make it match.
solve2 :: [Command] -> String
solve2 commands = show . (\s -> getHSub s * getDepthSub s) . foldl (flip foldCommand) zeroSub $ commands

-- This is utility to parse the input
parse ::  String -> Maybe [Command]
parse s = let ts = resolveLayout True $ myLexer s
        in case pListCommand ts of
           Bad s    -> Nothing
           Ok  tree -> Just tree

main :: IO ()
main = do
    -- Get the file from the command args
    args <- getArgs
    commands' <- case args of
        -- Read the file then parse it. fmap makes parse work on a
        -- side-effectful operation like readFile
        (x:_) -> fmap parse (readFile x)
    -- Consume the maybe and exit if it's Nothing
    commands <- maybe exitFailure pure commands'
    -- Solve part 1 and output it
    let a1 = solve1 commands
    putStrLn a1
    let a2 = solve2 commands
    putStrLn a2
