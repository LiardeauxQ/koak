module Lexer
    ( char
    , digit
    , digits
    , letter
    , letters
    , between
    , parens
    , oneOf
    , noneOf
    , manyOf
    , manyNoneOf
    , string
    , Parser.Parser(..)
    , Parser.many1
    , Parser.chainl
    , Parser.chainl1
    )
where

import qualified Parser
import           Control.Applicative

char :: Char -> Parser.Parser Char
char s = Parser.sc *> Parser.char s

letter :: Parser.Parser Char
letter = Parser.sc *> Parser.letter

letters :: Parser.Parser String
letters = Parser.sc *> Parser.many1 Parser.letter

digit :: Parser.Parser Char
digit = Parser.sc *> Parser.digit

digits :: Parser.Parser String
digits = Parser.sc *> Parser.many1 Parser.digit

between
    :: Parser.Parser open
    -> Parser.Parser close
    -> Parser.Parser a
    -> Parser.Parser a
between o c p = (Parser.sc *> o) *> (Parser.sc *> p) <* (Parser.sc *> c)

parens :: Parser.Parser a -> Parser.Parser a
parens = between (char '(') (char ')')

oneOf :: String -> Parser.Parser Char
oneOf s = Parser.sc *> Parser.oneOf s

manyOf :: String -> Parser.Parser String
manyOf s = Parser.sc *> Parser.manyNoneOf s

noneOf :: String -> Parser.Parser Char
noneOf s = Parser.sc *> Parser.noneOf s

manyNoneOf :: String -> Parser.Parser String
manyNoneOf s = Parser.sc *> Parser.manyNoneOf s

string :: String -> Parser.Parser String
string s = Parser.sc *> Parser.string s
