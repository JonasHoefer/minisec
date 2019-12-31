{-# LANGUAGE DeriveFunctor, TupleSections, LambdaCase #-}

module Text.Minisec where

import           Data.Char
import           Data.Functor
import           Data.Foldable                  ( asum )

import           Control.Applicative
import           Control.Monad


-- marks wether the parser did or did not consume input
-- Allows to implement <|> without capturing the current input [s], which gives better cotrol over the memory.
data Consumed a = Consumed a | Empty a deriving (Show, Functor)

unConsumed :: Consumed a -> a
unConsumed (Consumed x) = x
unConsumed (Empty    x) = x


newtype Parser s e a = Parser { runParser :: [s] -> Consumed (Either (e, [s]) (a, [s])) } deriving (Functor)

instance Applicative (Parser s e) where
    pure  = return

    (<*>) = ap


instance Monoid e => Alternative (Parser s e) where
    p <|> q = Parser $ \ss -> case runParser p ss of
        Empty (Left (e, ss')) -> runParser q ss' <&> \case
            Left  (e', ss'') -> Left (e <> e', ss'')
            Right x          -> Right x
        x -> x

    empty = Parser $ Empty . Left . (mempty, )


instance Monad (Parser s e) where
    return x = Parser $ Empty . Right . (x, )

    p >>= f = Parser $ \ss -> case runParser p ss of
        Empty    (Left  e       ) -> Empty (Left e)
        Consumed (Left  e       ) -> Consumed (Left e)
        Empty    (Right (x, ss')) -> runParser (f x) ss'
        Consumed (Right (x, ss')) ->
            Consumed . unConsumed $ runParser (f x) ss'


try :: Parser s e a -> Parser s e a
try p = Parser $ \ss -> case runParser p ss of
    Consumed (Left (e, _)) -> Empty (Left (e, ss)) -- rollback
    x                      -> x

next :: Monoid e => Parser s e s
next = Parser $ \case
    []       -> Empty (Left (mempty, []))
    (x : xs) -> Consumed (Right (x, xs))

satisfy :: Monoid e => (s -> Bool) -> Parser s e s
satisfy p = try $ next >>= \x -> if p x then return x else empty

char :: (Eq s, Monoid e) => s -> Parser s e s
char c = satisfy (== c)

string :: (Eq s, Monoid e) => [s] -> Parser s e [s]
string = foldr (\c -> (<*>) ((:) <$> satisfy (== c))) (return [])

digit :: Monoid e => Parser Char e Integer
digit = toInteger . digitToInt <$> satisfy isDigit

number :: Monoid e => Parser Char e Integer
number = foldl (\a d -> 10 * a + d) <$> pure 0 <*> some digit

chainl
    :: (Eq s, Monoid e)
    => Parser s e a
    -> Parser s e (a -> a -> a)
    -> Parser s e a
chainl p op = foldl (\a (op, b) -> op a b) <$> p <*> many ((,) <$> op <*> p)

expression
    :: (Eq s, Monoid e)
    => [[Parser s e (a -> a -> a)]]
    -> Parser s e a
    -> Parser s e a
expression opss atom = foldl (\p ops -> chainl p $ asum ops) atom opss

parens :: Monoid e => Parser Char e a -> Parser Char e a
parens p = char '(' *> p <* char ')'

infix  0 <?>
(<?>) :: Monoid e => Parser s e a -> e -> Parser s e a
p <?> e = Parser $ \s -> case runParser p s of
    Consumed (Left (e', s')) -> Consumed (Left (e <> e', s'))
    Empty    (Left (e', s')) -> Empty (Left (e <> e', s'))
    r                        -> r

