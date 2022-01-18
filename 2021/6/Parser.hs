{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import Data.Void (Void)
import qualified Text.Megaparsec as P
import Control.Monad.Combinators (sepBy1)
import Lexer (Token(..), isDigit)
import Types (Fish(..), Days, daysFromFishes)

type Parser = P.Parsec Void [Token]

fish :: Parser Fish
fish = do
    (Digit x) <- P.satisfy isDigit
    return $ Fish x

fishes :: Parser [Fish]
fishes = sepBy1 fish (P.single Separator)

days :: Parser Days
days = daysFromFishes <$> fishes

parse :: Parser Days
parse = days
