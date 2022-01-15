module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Control.Monad (join)
import Data.Ord (comparing)
import Data.List (elemIndex, maximumBy, minimumBy, (\\), nub)
import Data.Maybe (fromJust)
import qualified Data.Text.IO as T
import Text.Megaparsec (parseMaybe)
import qualified Parser
import qualified Lexer
import Parser (Parsed(..), Card(..), Bingo(..), Call(..))

solve1 parsed = finalScoreCard calls scoreCalls winningCard * winningCall
    where
        calls = getCalls parsed
        cards = getCards parsed
        winningCard = scoreCards scoreCalls cards
        scoreCalls = invertCalls calls
        calledAt = scoreBingo scoreCalls $ scoreCard scoreCalls winningCard
        winningCall = calls !! (fromIntegral calledAt)

-- This is identical to solve1 except it uses scoreCards2
solve2 parsed = finalScoreCard calls scoreCalls winningCard * winningCall
    where
        calls = getCalls parsed
        cards = getCards parsed
        winningCard = scoreCards2 scoreCalls cards
        scoreCalls = invertCalls calls
        calledAt = scoreBingo scoreCalls $ scoreCard scoreCalls winningCard
        winningCall = calls !! (fromIntegral calledAt)

finalScoreCard :: [Call] -> [Call] -> Card -> Call
finalScoreCard calls scoreCalls card = sum $ cardCalls \\ neededCalls
    where
        neededCalls = take (succ . fromIntegral . scoreBingo scoreCalls . scoreCard scoreCalls $ card) calls
        cardCalls = nub . join . fmap unbingo . getBingos $ card

-- This is identical to scoreCards2 except it uses maximumBy
scoreCards2 :: [Call] -> [Card] -> Card
scoreCards2 scoreCalls cards = maximumBy (comparing (scoreBingo scoreCalls . scoreCard scoreCalls)) cards

scoreCards :: [Call] -> [Card] -> Card
scoreCards scoreCalls cards = minimumBy (comparing (scoreBingo scoreCalls . scoreCard scoreCalls)) cards

scoreCard :: [Call] -> Card -> Bingo
scoreCard scoreCalls (Card bingos) = minimumBy (comparing (scoreBingo scoreCalls)) bingos

scoreBingo :: [Call] -> Bingo -> Call
scoreBingo scoreCalls (Bingo calls) = maximumBy (comparing id) . fmap (\(Call e) -> scoreCalls !! (fromIntegral e)) $ calls

invertCalls :: [Call] -> [Call]
invertCalls calls = reverse . foldl (\l e -> (Call . fromIntegral . fromJust . elemIndex e $ calls):l) [] $ empty
    where
        empty = [0..(maximum calls)]

parse i = parseMaybe Lexer.lexer i >>= parseMaybe Parser.parse

parseFile name = fmap parse (T.readFile name)

main :: IO ()
main = do
    -- Get the file from the command args
    args <- getArgs
    parsed' <- case args of
        -- Read the file then parse it. fmap makes parse work on a
        -- side-effectful operation like readFile
        (x:_) -> parseFile x
    -- Consume the maybe and exit if it's Nothing
    parsed <- maybe exitFailure pure parsed'
    let s1 = solve1 parsed
    putStr "Solve1: "
    print s1
    let s2 = solve2 parsed
    putStr "Solve2: "
    print s2
