module ExprParser
    ( stmt
    )
where

import           Parser
import           Control.Monad
import           Control.Applicative
import           Expression

stmt = many kdefs

kdefs :: Parser ()
kdefs =
    (string "def" *> defs <* char ';')
        <|> (expressions >> char ';' *> return ())

defs :: Parser ()
defs = do
    prototype
    expressions
    return ()

prototype :: Parser ()
prototype = do
    s <- prototypeFunc
    prototypeArgs
    return ()

prototypeFunc :: Parser ()
prototypeFunc =
    return
        $   (do
                string "unary"
                optional decimalConst
            )
        <|> (do
                string "binary"
                optional decimalConst
            )
        <|> Just
        .   Var
        <$> identifier


prototypeArgs :: Parser ()
prototypeArgs = do
    types <- parens $ many
        (do
            id <- identifier
            char ':'
            t <- typ
            return $ id ++ ":" ++ t
        )
    char ':'
    retType <- typ
    return ()

typ :: Parser String
typ = string "int" <|> string "double" <|> string "void"

expressions :: Parser Expr
expressions =
    forExpr <|> ifExpr <|> whileExpr <|> (expression <* manyExpressions)

manyExpressions :: Parser [Expr]
manyExpressions = many (char ':' *> expression)

forExpr :: Parser Expr
forExpr = do
    string "for"
    ident <- identifier
    char '='
    expr <- expression
    char ','
    ident2 <- identifier
    char '<'
    expr2 <- expression
    char ','
    expr3 <- expression
    string "in"
    expr4 <- expressions
    return $ For ident expr ident2 expr2 expr3 expr4

ifExpr :: Parser Expr
ifExpr = do
    string "if"
    expr <- expression
    string "then"
    expr1 <- expressions
    expr2 <- optional (string "else" >> expressions)
    return $ If expr expr1 expr2

whileExpr :: Parser Expr
whileExpr = do
    string "while"
    expr <- expression
    string "do"
    expr1 <- expressions
    return $ While expr expr1

binop :: Parser (Expr -> Expr -> Expr)
binop =
    (oneOf "+-/*%<>=" >>= \c -> return $ BinaryOp (c : []))
        <|> (string "==" >>= return . BinaryOp)
        <|> (string "!=" >>= return . BinaryOp)

unop :: Parser (Expr -> Expr)
unop = oneOf "-!" >>= return . UnaryOp . (: [])

expression :: Parser Expr
expression = do
    un  <- unary
    mul <- many (unop >> (unary <|> expression))
    return $ Identifier "expression"

unary :: Parser Expr
unary =
    do
            op <- unop
            un <- unary
            return $ Identifier "op + unary"
        <|> postfix

postfix :: Parser Expr
postfix = do
    p <- primary
    c <- optional callExpr
    return $ Identifier "postfix"

callExpr :: Parser Expr
callExpr = parens $ callParamExpr

callParamExpr :: Parser Expr
callParamExpr = do
    optional $ do
        e      <- expression
        others <- many (char ',' >> expression)
        Call e others

primary :: Parser Expr
primary = (Var <$> identifier) <|> literal <|> parens expressions

identifier :: Parser String
identifier = do
    first <- letter
    next  <- many (letter <|> digit)
    return (first : next)

dot :: Parser Char
dot = char '.' <* noneOf "."

decimalConst :: Parser Expr
decimalConst = Int . read <$> many1 digit

doubleConst :: Parser Expr
doubleConst =
    do
            Int f <- decimalConst
            dot
            s <- many digit
            return $ Float $ read $ (show f) ++ "." ++ s
        <|> do
                dot
                Int v <- decimalConst
                return $ Float $ read $ "0." ++ (show v)


literal :: Parser Expr
literal = decimalConst <|> doubleConst
