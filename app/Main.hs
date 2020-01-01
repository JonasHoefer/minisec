module Main where

import           Data.Functor
import           Control.Applicative

import           Text.Minisec
import           Text.Minisec.String


expr :: Parser Char String Integer
expr = expression
    [[char '*' $> (*), char '/' $> div], [char '+' $> (+), char '-' $> (-)]]
    (number <|> parens expr <?> "Expected a number or parentheses")

main :: IO ()
main = getLine >>= \s -> print $ runParser expr s
