{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser
    ( Parser(..)
    , ParseError(..)
    , runParser
    , manyNoneOf
    , sepBy1
    , sepBy
    , letter
    , char
    , chainl
    , chainl1
    , string
    , anyChar
    , between
    , oneOf
    , noneOf
    , digit
    , many1
    , parens
    , sc
    , manyOf
    )
where

import           State
import           Data.Either
import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import           Control.Monad.Fail
import           Data.Bifunctor

data ParseError = ParseError { line :: Int
                             , column :: Int
                             , message :: String
                             } deriving(Show, Eq)

-- Wrapping StateT with a newtype.
newtype Parser a = Parser { unParser :: StateT String (Either ParseError) a }
-- To implement monad on Parser we need to unwrap the parser with unParser
-- then use underlying StateT implementation.

instance MonadFail Parser where
    fail s = Parser $ StateT $ \c ->
        Left $ ParseError { line = 0, column = 0, message = s }

instance Functor Parser where
    fmap f p = Parser $ f <$> unParser p

instance Applicative Parser where
    pure a = Parser $ pure a
    f <*> a = Parser $ unParser f <*> unParser a

instance Monad Parser where
    return a = Parser $ return a
    a >>= f = Parser $ StateT $ \s -> do
        (a', s') <- runParser a s
        runParser (f a') s'

instance Alternative Parser where
    empty = Parser $ StateT $ \s ->
        Left $ ParseError { line = 0, column = 0, message = "Empty" }
    a <|> b = Parser $ StateT $ \s -> case (runParser a s, runParser b s) of
        (Right x, _      ) -> Right x
        (_      , Right y) -> Right y
        (x      , _      ) -> x

runParser :: Parser a -> String -> Either ParseError (a, String)
runParser = runState . unParser

many1 :: Parser a -> Parser [a]
many1 p = do
    x  <- p
    xs <- many p
    return (x : xs)

anyChar :: Parser Char
anyChar = Parser $ StateT $ \case
    ""       -> Left $ ParseError { line = 0, column = 0, message = "Empty" }
    (c : cs) -> Right (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
    c <- anyChar
    guard $ pred c
    return c

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

noneOf :: String -> Parser Char
noneOf cs = Parser $ StateT $ \case
    (x : xs) -> if x `elem` cs
        then Left $ ParseError { line = 0, column = 0, message = "Empty" }
        else Right (x, xs)
    [] -> Left $ ParseError { line = 0, column = 0, message = "Empty" }

oneOf :: String -> Parser Char
oneOf cs = Parser $ StateT $ \case
    (x : xs) -> if x `elem` cs
        then Right (x, xs)
        else Left $ ParseError { line = 0, column = 0, message = "Empty" }
    [] -> Left $ ParseError { line = 0, column = 0, message = "Empty" }

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

sepBy p sep = sepBy1 p sep <|> return []

sepBy1 p sep = do
    x  <- p
    xs <- many (sep >> p)
    return (x : xs)

sc :: Parser ()
sc = void $ manyOf "\t \n"

digit :: Parser Char
digit = oneOf "0123456789"

letter :: Parser Char
letter = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

between :: Parser open -> Parser close -> Parser a -> Parser a
between o c p = o *> p <* c

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 x y = x >>= rest
    where rest a = (y >>= \f -> x >>= \b -> rest (f a b)) <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

manyOf :: String -> Parser String
manyOf = many . oneOf

manyNoneOf :: String -> Parser String
manyNoneOf = many . noneOf
