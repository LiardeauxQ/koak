module KParser
    ( koak
    , kdefs
    , defs
    , prototype
    , prototypeFunc
    , prototypeArgs
    , ktype
    , expressions
    , forExpr
    , ifExpr
    , whileExpr
    , binop
    , unop
    , expression
    , unary
    , postfix
    , callExpr
    , primary
    , identifier
    , dot
    , decimalConst
    , doubleConst
    , literal
    )
where

import           Debug.Trace
import           Lexer
import           Control.Monad
import           Control.Applicative
import           AST

koak :: Parser [KDefs]
koak = many1 kdefs

kdefs :: Parser KDefs
kdefs =
    (string "def" *> defs <* char ';')
        <|> (Expressions <$> (expressions <* char ';'))

defs :: Parser KDefs
defs = prototype <*> expressions

prototype :: Parser (KExprs -> KDefs)
prototype =
    Def <$> prototypeFunc <*> prototypeArgs <*> optional (char ':' >> ktype)

prototypeFunc :: Parser Name
prototypeFunc = identifier


prototypeArgs :: Parser [VariableDef]
prototypeArgs = parens $ many $ do
    name <- identifier
    char ':'
    valueType <- optional ktype
    return $ VariableDef name valueType


ktype :: Parser KType
ktype =
    (TInteger <$ string "int")
        <|> (TDouble <$ string "double")
        <|> (TVoid <$ string "void")


expressions :: Parser KExprs
expressions =
    forExpr
        <|> ifExpr
        <|> whileExpr
        <|> Expression
        <$> ((:) <$> expression <*> many (char ':' *> expression))

forExpr :: Parser KExprs
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
    For (Identifier ident) expr (Identifier ident2) expr2 expr3 <$> expressions

ifExpr :: Parser KExprs
ifExpr = do
    string "if"
    expr <- expression
    string "then"
    expr1 <- expressions
    expr2 <- optional (string "else" >> expressions)
    return $ If expr expr1 expr2

whileExpr :: Parser KExprs
whileExpr = do
    string "while"
    expr <- expression
    string "do"
    While expr <$> expressions

binop :: Parser (KExpr -> KExpr -> KExpr)
binop =
    (BinaryOp <$> string "==")
        <|> (BinaryOp <$> string "!=")
        <|> (BinaryOp . (: []) <$> oneOf "+-/*%<>=")

unop :: Parser (KExpr -> KExpr)
unop = UnaryOp . (: []) <$> oneOf "-!"

assop = BinaryOp "=" <$ char '='
eqop = (BinaryOp <$> string "==") <|> (BinaryOp <$> string "!=")
cmpop = (BinaryOp "<" <$ char '<') <|> (BinaryOp ">" <$ char '>')
addop = (BinaryOp "+" <$ char '+') <|> (BinaryOp "-" <$ char '-')
mulop = (BinaryOp "*" <$ char '*') <|> (BinaryOp "/" <$ char '/')

expr = eq `chainl1` assop

eq = cmp `chainl1` eqop

cmp = add `chainl1` cmpop

add = mul `chainl1` addop

mul = (unary <|> primary) `chainl1` mulop

expression :: Parser KExpr
expression = expr

unary :: Parser KExpr
unary = (unop <*> unary) <|> postfix

postfix :: Parser KExpr
postfix = do
    res  <- optional callExpr
    expr <- primary
    return $ case res of
        Nothing -> expr
        Just x  -> Call expr x

callExpr :: Parser [KExpr]
callExpr = parens $ sepBy1 expression (char ',')

primary :: Parser KExpr
primary =
    (Identifier <$> identifier) <|> literal <|> (Primary <$> parens expressions)

identifier :: Parser String
identifier = do
    first <- letter
    next  <- many (letter <|> digit)
    return (first : next)

dot :: Parser Char
dot = char '.'

decimalConst :: Parser KExpr
decimalConst = Int . read <$> many1 digit

doubleConst :: Parser KExpr
doubleConst =
    (do
            Int f <- decimalConst
            dot
            s <- many digit
            return $ Float $ read $ show f ++ "." ++ s
        )
        <|> (do
                dot
                Int v <- decimalConst
                return $ Float $ read $ "0." ++ show v
            )


literal :: Parser KExpr
literal = doubleConst <|> decimalConst
