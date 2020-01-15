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

-- Function defintion
--
-- def test(x: double) : double x + 2.0;
-- (Def
--   (DefineFunction
--     "test"
--     [ Variable "x" (Just Double) ]
--     (Just Double)
--     (BinaryOp
--       "+"
--       (Var "x")
--       (Float 2.0)
--     )
--   )
-- )


-- Function call
--
-- test(5.0);
--
-- (Call
--   "test"
--   [ Float 5.0 ]
-- )
--

-- Complexe expressions
--
-- def test(x: double): double x - 1.0;
-- y = 2.0;
-- while y < 10 do y = y * 2;
-- test(y + 3.0);
--
-- [
--   (Def
--     (DefineFunction
--       "test"
--       [ Variable "x" (Just Double) ]
--       (Just Double)
--       (BinaryOp
--         "+"
--         (Var "x")
--         (Float 2.0)
--       )
--     )
--   )
--   ,
--   (Expr
--     (BinaryOp
--       "="
--       (Var "y")
--       (Double 2.0)
--     )
--   ),
-- ]

-- data Expression = Int Integer
--                 | Float Double
--                 | If Expr Expr (Maybe Expr)
--                 | While Expr Expr
--           | For Identifier Expr Identifier Expr Expr Expr
--           | UnaryOp Identifier Expr
--           | BinaryOp Identifier Expr Expr
--           | Function Identifier [(String, Maybe Type)] Expr (Maybe Type)
--           | Call Identifier [Expr]

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
