{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import Data.Void (Void)
import qualified Text.Megaparsec as P
import Control.Monad.Combinators (sepEndBy1)
import Lexer (Token(..), isDigit)
import Types (LineSegment(..), Point(..))

type Parser = P.Parsec Void [Token]

data Parsed = Parsed { getSegments :: [LineSegment] }
    deriving (Show)

point :: Parser Point
point = do
    (Digit x) <- P.satisfy isDigit
    P.single Separator
    (Digit y) <- P.satisfy isDigit
    return $ Point x y

lineSegment :: Parser LineSegment
lineSegment = do
    p1 <- point
    P.single Arrow
    p2 <- point
    return $ LineSegment p1 p2

lineSegments :: Parser [LineSegment]
lineSegments = sepEndBy1 lineSegment (P.single Newline)

parse :: Parser Parsed
parse = Parsed <$> lineSegments
