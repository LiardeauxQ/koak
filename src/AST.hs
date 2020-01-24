module AST where

import           Data.Maybe

type Name = String

data KType = TInteger
           | TDouble
           | TVoid
           deriving(Show, Eq, Ord)

data KDefs = Def Name [VariableDef] (Maybe KType) KExprs
           | Expressions KExprs
           deriving(Show, Eq, Ord)

data VariableDef = VariableDef Name (Maybe KType) deriving(Show, Eq, Ord)

data KExpr = Int Integer
           | Float Double
           | BinaryOp Name KExpr KExpr
           | UnaryOp Name KExpr
           | Identifier Name
           | Call KExpr [KExpr]
           | Primary KExprs
           deriving(Show, Eq, Ord)

data KExprs = For KExpr KExpr KExpr KExpr KExpr KExprs
            | If KExpr KExprs (Maybe KExprs)
            | While KExpr KExprs
            | Expression [KExpr]
            deriving(Show, Eq, Ord)

type OperatorPrecendence = (String, Int)

binaryOpPrecedence :: [OperatorPrecendence]
binaryOpPrecedence =
    [ ("*" , 40)
    , ("/" , 40)
    , ("+" , 30)
    , ("-" , 30)
    , ("<" , 20)
    , (">" , 20)
    , ("==", 10)
    , ("!=", 10)
    , ("=" , 1)
    ]

getOperatorPrecendence :: String -> Int
getOperatorPrecendence s = fromMaybe 0 $ lookup s binaryOpPrecedence
