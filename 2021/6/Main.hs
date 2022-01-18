module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Control.Monad (join)
import qualified Data.Text.IO as T
import Text.Megaparsec (parseMaybe)
import qualified Parser
import qualified Lexer
import Solve1 (solve1)
import Solve2 (solve2)

lexInput = parseMaybe Lexer.lexer
parse i = lexInput i >>= parseMaybe Parser.parse

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
    days <- maybe exitFailure pure parsed'
    let s1 = solve1 days
    putStr "Solve1: "
    print s1
    let s2 = solve2 days
    putStr "Solve2: "
    print s2
