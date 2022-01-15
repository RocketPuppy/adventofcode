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
import Control.Monad.Combinators (skipMany)
import Text.Megaparsec.Debug (dbg)

type Lexer = P.Parsec Void T.Text

data Token
    = Digit Integer
    | GenericSeparator
    | ColumnSeparator
    | RowSeparator
    | CardSeparator
    deriving (Show, Eq, Ord)

isDigit (Digit _) = True
isDigit _ = False

digit :: Lexer Token
digit = Digit <$> L.decimal

genericSeparator :: Lexer Token
genericSeparator = C.char ',' *> pure GenericSeparator

columnSeparator :: Lexer Token
columnSeparator = C.hspace1 *> pure ColumnSeparator

rowSeparator :: Lexer Token
rowSeparator = C.newline *> pure RowSeparator

-- If cardSeparator fails it will consume the newline that rowSeparator should consume
cardSeparator :: Lexer Token
cardSeparator = P.try (C.newline *> C.newline) *> pure CardSeparator

lex :: Lexer Token
lex = digit <|> genericSeparator <|> cardSeparator <|> columnSeparator <|> rowSeparator

lexer :: Lexer [Token]
lexer = P.manyTill lex P.eof
