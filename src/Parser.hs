{-# LANGUAGE InstanceSigs #-}
module Parser
    ( Parser(..)
    , runParser
    , manyNoneOf
    , evalParser
    , sepBy1
    , sepBy
    , letter
    , char
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

import           Control.Monad
import           Control.Applicative
import           Data.Maybe

newtype Parser a = Parser (String -> Maybe (a, String))

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ fmap (\(a, s') -> (f a, s')) . runParser p

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (Parser mf) <*> (Parser mx) =
        Parser $ \s ->
            mf s >>= \(f, s') -> mx s' >>= \(x, s'') -> pure (f x, s'')

{- Do notation implementation of (<*>) operator:

     (Parser mf) <*> (Parser mx)  = Parser $ \s ->
        (f, s') <- mf s
        (x, s'') <- mx 's
        return (f x, s'')

-}

instance Monad Parser where
    return a = Parser $ \s -> return (a, s)
    a >>= f = Parser $ \s -> runParser a s >>= \(a', s') -> runParser (f a') s'

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser x <|> Parser y = Parser $ \s -> x s <|> y s

many1 :: Parser a -> Parser [a]
many1 p = do
    x  <- p
    xs <- many p
    return (x : xs)

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser a) = a

evalParser :: Parser a -> String -> Either String a
evalParser p s = case runParser p s of
    Just (a, "") -> Right a
    Just (a, s ) -> Left $ "Cannot parse not empty:" ++ s ++ "."
    Nothing      -> Left "invalid syntax"

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
    ""       -> empty
    (c : cs) -> return (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
    c <- anyChar
    guard $ pred c
    return c

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string []       = pure []
string (c : cs) = (:) <$> char c <*> string cs

noneOf :: String -> Parser Char
noneOf cs = Parser $ \s -> case s of
    (x : xs) -> if x `elem` cs then Nothing else Just (x, xs)
    []       -> Nothing

oneOf :: String -> Parser Char
oneOf cs = Parser $ \s -> case s of
    (x : xs) -> if x `elem` cs then Just (x, xs) else Nothing
    []       -> Nothing

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

sepBy p sep = sepBy1 p sep <|> return empty

sepBy1 p sep = do
    x  <- p
    xs <- many (sep >> p)
    return (x : xs)

sc :: Parser ()
sc = void $ manyOf "\t "

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

-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- chainl1 p op = p >>= rest
--     where
--         rest a = (do
--             f <- op
--             b <- p
--             rest (f a b)) <|> pure a
