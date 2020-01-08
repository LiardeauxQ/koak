module Expression
    ( Expr(..)
    )
where

data Expr = Number Int
          | Floating Double
          | If Expr Expr (Maybe Expr)
          | While Expr Expr
          | For String Expr String Expr Expr Expr
          | UnaryOp Char Expr
          | BinaryOp Char Expr Expr
          | Identifier String
          | Bloc [Expr]

data Type = Integer
          | Double
          | Void

instance Show Expr where
    show (Number   a) = show a
    show (Floating a) = show a
    show (If a b c  ) = show a ++ show b ++ show c
    show (While a b ) = show a ++ show b
    show (For a b c d e f) =
        show a ++ show b ++ show c ++ show d ++ show e ++ show f
    show (UnaryOp a b   ) = show a ++ show b
    show (BinaryOp a b c) = show a ++ show b ++ show c
