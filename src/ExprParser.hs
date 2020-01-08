module ExprParser
    ( stmt
    )
where

import           Parser
import           Control.Monad
import           Control.Applicative
import           Expression

stmt = many kdefs

kdefs = (string "def" >> defs >> char ';') <|> (expressions >> char ';')

defs = prototype >> expressions

prototype =
    (   (string "unary" >> maybe empty (optional decimalConst))
        <|> (string "binary" >> maybe empty (optional decimalConst))
        <|> identifier
        )
        prototypeArgs

prototypeArgs :: Parser Expr
prototypeArgs = do
    parens $ many
        (do
            id <- identifier
            char ':'
            t <- typ
            return $ TypeDeclaration id t
        )
    char ':'
    retType <- typ

typ :: Parser String
typ = string "int" <|> string "double" <|> string "void"

expression :: Parser Expr
expressions =
    forExpr
        <|> ifExpr
        <|> whileExpr
        <|> (expression >> (many (char ':' >> expression)))

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
    exprs <- optional (string "else" >> expressions)
    return $ If expr expr1 exprs

whileExpr :: Parser Expr
whileExpr = do
    string "while"
    expr <- expression
    string "do"
    expr1 <- expressions
    return $ While expr expr1

binop = oneOf "+-/*%<>=" <|> string "^" <|> string "$" -- Change with operators
unop = oneOf "-!"

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

callExpr = parens $ optional (expression >> many (char ',' >> expression))

primary :: Parser Expr
primary = (Identifier <$> identifier) <|> literal <|> parens expressions

identifier :: Parser String
identifier = do
    first <- letter
    next  <- many (letter <|> digit)
    return (first : next)

dot = char '.' >> noneOf "."

decimalConst :: Parser Expr
decimalConst = Number . read <$> many1 digit

doubleConst :: Parser Expr
doubleConst =
    do
            Number f <- decimalConst
            dot
            s <- many digit
            return $ Floating $ read $ (show f) ++ "." ++ s
        <|> do
                dot
                Number v <- decimalConst
                return $ Floating $ read $ "0." ++ (show v)


literal :: Parser Expr
literal = decimalConst <|> doubleConst
