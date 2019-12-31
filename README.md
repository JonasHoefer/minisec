# Minisec

A minimalistic parser combinator library. Mostly usefull for understanding the basic concepts of parser combinators and their implementation.


## Example

An operator precedence parser for arithmetic expressions:

```Haskell
expr :: Parser Char String Integer
expr = expression
    [ [char '*' $> (*), char '/' $> div]
    , [char '+' $> (+), char '-' $> (-)] ]
    (number <|> parens expr <?> "Expected a number or parentheses")

main :: IO ()
main = getLine >>= \s -> print $ runParser expr s
```
