module Expression where

import           Data.Maybe

type Name = String

data KType = TInteger
           | TDouble
           | TVoid
           deriving(Show, Eq, Ord)

data KDefs = Def String [VariableDef] (Maybe KType) KExprs
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

-- ```
-- c: double = 2.0
-- c = 2.0
-- def mandelhelp(xmin xmax xstep ymin ymax ystep)
--       for y = ymin, y < ymax, ystep in (
--           (for x = xmin, x < xmax, xstep in
--               printdensity(mandelconverge(x,y)))
--           : putchard(10))
-- ```
function = Def
    "madelhelp"
    [ VariableDef "xmin"  Nothing
    , VariableDef "xmax"  Nothing
    , VariableDef "xmax"  Nothing
    , VariableDef "xstep" Nothing
    , VariableDef "ymax"  Nothing
    , VariableDef "ystep" Nothing
    ]
    Nothing
    (For
        (Identifier "y")
        (Identifier "ymin")
        (Identifier "y")
        (Identifier "ymax")
        (Identifier "ystep")
        (Expression
            [ Primary
                  (For
                      (Identifier "x")
                      (Identifier "xmin")
                      (Identifier "x")
                      (Identifier "xmax")
                      (Identifier "xstep")
                      (Expression
                          [ Call
                              (Identifier "printdensity")
                              [ Call (Identifier "mandelconverge")
                                     [Identifier "x", Identifier "y"]
                              ]
                          , Call (Identifier "putchar") [Int 10]
                          ]
                      )
                  )
            ]
        )
    )




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
