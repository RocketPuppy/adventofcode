{-# LANGUAGE OverloadedStrings #-}
module Lexer (
    Token(..),
    lex,
    lexer,
    isDigit
) where

import Prelude hiding (lex)
import Data.Void (Void)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (empty, (<|>))

type Lexer = P.Parsec Void T.Text

data Token
    = Digit Int
    | Separator
    | Arrow
    | Newline
    deriving (Show, Eq, Ord)

space = L.space C.hspace1 empty empty
lexeme = L.lexeme space
symbol = L.symbol space

isDigit (Digit _) = True
isDigit _ = False

digit :: Lexer Token
digit = Digit <$> lexeme L.decimal

separator :: Lexer Token
separator = symbol "," *> pure Separator

arrow :: Lexer Token
arrow = symbol "->" *> pure Arrow

newline :: Lexer Token
newline = C.newline *> pure Newline

lex :: Lexer Token
lex = digit <|> separator <|> arrow <|> newline

lexer :: Lexer [Token]
lexer = P.manyTill lex P.eof
