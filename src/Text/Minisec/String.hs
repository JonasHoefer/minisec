module Text.Minisec.String where

import           Data.Char
import           Data.Functor

import           Control.Applicative
import           Control.Monad

import           Text.Minisec


digit :: Monoid e => Parser Char e Integer
digit = toInteger . digitToInt <$> satisfy isDigit

number :: Monoid e => Parser Char e Integer
number = foldl (\a d -> 10 * a + d) <$> pure 0 <*> some digit

parens :: Monoid e => Parser Char e a -> Parser Char e a
parens p = char '(' *> p <* char ')'

alpha :: Monoid e => Parser Char e Char
alpha = satisfy isAlpha

alphaNum :: Monoid e => Parser Char e Char
alphaNum = satisfy isAlphaNum

lower :: Monoid e => Parser Char e Char
lower = satisfy isLower

identifier :: Monoid e => Parser Char e String
identifier = (:) <$> lower <*> many alphaNum

whitespace :: Monoid e => Parser Char e ()
whitespace = (many $ satisfy isSeparator) $> ()

whitespace1 :: Monoid e => Parser Char e ()
whitespace1 = (some $ satisfy isSeparator) $> ()
