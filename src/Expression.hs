module Expression
    ( Expr(..)
    , Type(..)
    , Identifier
    , getOperatorPrecendence
    )
where

import           Data.Maybe

type Identifier = String

data Type = Integer
          | Double
          | Void
          deriving(Show)

data Expr = Int Integer
          | Float Double
          | Var String
          | If Expr Expr (Maybe Expr)
          | While Expr Expr
          | For Identifier Expr Identifier Expr Expr Expr
          | UnaryOp Identifier Expr
          | BinaryOp Identifier Expr Expr
          | Function Identifier [Identifier] Expr

instance Show Expr where
    show (Int   a  ) = show a
    show (Float a  ) = show a
    show (If a b c ) = show a ++ show b ++ show c
    show (While a b) = show a ++ show b
    show (For a b c d e f) =
        show a ++ show b ++ show c ++ show d ++ show e ++ show f
    show (UnaryOp a b   ) = show a ++ show b
    show (BinaryOp a b c) = show a ++ show b ++ show c
    show (Function a b c) = "function: " ++ show a ++ " " ++ show b


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
