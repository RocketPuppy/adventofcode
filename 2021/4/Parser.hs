{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import Data.Void (Void)
import Data.List (transpose)
import qualified Text.Megaparsec as P
import Control.Monad.Combinators (sepBy1, sepEndBy1)
import Lexer (Token(..))

type Parser = P.Parsec Void [Token]

newtype Call = Call Integer
    deriving (Eq, Ord, Show, Num, Enum, Integral, Real)

newtype Bingo = Bingo { unbingo :: [Call] }
    deriving (Eq, Show)

newtype Card = Card { getBingos :: [Bingo] }
    deriving (Eq, Show)

data Parsed = Parsed { getCalls :: [Call], getCards :: [Card] }
    deriving (Show)

transposeBingos :: [Bingo] -> [Bingo]
transposeBingos bingos = fmap Bingo . transpose . fmap unbingo $ bingos

isDigit (Digit _) = True
isDigit _ = False

call :: Parser Call
call = (\(Digit d) -> Call d) <$> P.satisfy isDigit

calls :: Parser [Call]
calls = sepBy1 call (P.single GenericSeparator)

bingo :: Parser Bingo
bingo = Bingo <$> (P.optional (P.single ColumnSeparator) *> sepBy1 call (P.single ColumnSeparator))

bingos :: Parser [Bingo]
bingos = sepEndBy1 bingo (P.single RowSeparator)

card :: Parser Card
card = (\ bingos -> (Card (bingos ++ transposeBingos bingos))) <$> bingos

cards :: Parser [Card]
cards = sepBy1 card (P.single CardSeparator)

parse :: Parser Parsed
parse = do
    pCalls <- calls
    P.single CardSeparator
    pCards <- cards
    return (Parsed pCalls pCards)
