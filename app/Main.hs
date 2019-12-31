module Main where

import           Data.Functor
import           Control.Applicative
import           Text.Minisec


expr :: Parser Char String Integer
expr = expression
    [[char '*' $> (*), char '/' $> div], [char '+' $> (+), char '-' $> (-)]]
    (number <|> parens expr)

main :: IO ()
main = getLine >>= \s -> print $ runParser expr s
