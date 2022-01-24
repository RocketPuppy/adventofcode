{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import Data.Void (Void)
import qualified Text.Megaparsec as P
import Control.Monad.Combinators (sepBy1)
import Lexer (Token(..), isDigit)
import Types (Crab(..))

type Parser = P.Parsec Void [Token]

crab :: Parser Crab
crab = do
    (Digit x) <- P.satisfy isDigit
    return $ Crab x

crabs :: Parser [Crab]
crabs = sepBy1 crab (P.single Separator)

parse :: Parser [Crab]
parse = crabs
