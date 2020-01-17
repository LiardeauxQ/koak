module Expression where

import           Data.Maybe

data KoakType = Integer
              | Double
              | Void
              deriving(Show, Eq, Ord)

data KoakExpression = Def Defining
                    | Expr Expressions
                    deriving(Show, Eq, Ord)

data Variable = Variable String (Maybe KoakType) deriving(Show, Eq, Ord)

data Defining = DefineFunction String [Variable] (Maybe KoakType) Expressions
              | DefineExtern String [Variable] (Maybe KoakType) Expressions
              -- | DefineBinaryOp Operator Expression Expression
              -- | DefineUnaryOp Operator Expression
              deriving(Show, Eq, Ord)

data Expression = Int Integer
                | Float Double
                | BinaryOp String Expression Expression
                | UnaryOp String Expression
                | Var String
                | Call String [Expression]
                deriving(Show, Eq, Ord)

data Expressions = For String Expression Expression Expression Expressions
                 | While Expression Expressions
                 | If Expression Expressions
                 | Exprs [Expression]
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
