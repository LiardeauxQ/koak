module Expression
    ( Expr(..)
    , Type(..)
    , Identifier
    , Variable(..)
    , getOperatorPrecendence
    )
where

import           Data.Maybe

type Identifier = String

data Type = Integer
          | Double
          | Void
          deriving(Show)

data Variable = Variable { name :: String, typeName :: Maybe Type } deriving(Show)

data Expr = Int Integer
          | Float Double
          | Var Variable
          | If { condition :: Expr, body :: Expr, elseBody :: Maybe Expr }
          | While { condition :: Expr, body :: Expr }
          | For { iterator :: Identifier, beginValue :: Expr, iterator2 :: Identifier, endValue :: Expr, step :: Expr, body :: Expr }
          | UnaryOp Identifier Expr
          | BinaryOp Identifier Expr Expr
          | Function { functionName :: Identifier, args :: [Variable], body :: Expr, returnTypeName :: Maybe Type }
          | Call Identifier [Expr]

instance Show Expr where
    show (Int   a  ) = show a
    show (Float a  ) = show a
    show (If a b c ) = show a ++ show b ++ show c
    show (While a b) = show a ++ show b
    show (For a b c d e f) =
        show a ++ show b ++ show c ++ show d ++ show e ++ show f
    show (UnaryOp op b   ) = show op ++ show b
    show (BinaryOp op b c) = show b ++ " " ++ show op ++ " " ++ show c
    show Function { functionName = a, args = b, body = c, returnTypeName = d }
        = show a ++ "(" ++ show b ++ ") -> " ++ show d ++ " {" ++ show c ++ "}"
    show (Var Variable { name = x, typeName = y }) = show x ++ ": " ++ show y


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
