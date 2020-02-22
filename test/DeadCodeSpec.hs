module DeadCodeSpec where

import           TypeInference
import           AST
import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Debug.Trace
import           DeadCode

simpleFunctionNoDeadCode =
    -- def test(x, y, z: int) x + y + 2.0;
    -- test(1, 2, 3);
    [ Def
        "test"
        [ VariableDef "x" Nothing
        , VariableDef "y" Nothing
        , VariableDef "z" (Just TInteger)
        ]
        Nothing
        (Expression
            [ BinaryOp "+"
                       (Float 2.0)
                       (BinaryOp "+" (Identifier "x") (Identifier "y"))
            ]
        )
    , Expressions $ Expression [Call (Identifier "test") [Int 1, Int 2, Int 3]]
    ]

multipleValuesNoDeadCode =
    -- a = 50;
    -- def add(x, y): double x + y;
    -- i = 0.0;
    -- f = add(2.0, i);
    -- while i < a do
    --     b = i + 10 :
    --     print(b + 10);
    -- add(a);
    -- addInt(i);
    --
    [ Expressions $ Expression [BinaryOp "=" (Identifier "a") (Int 50)]
    , Def "add"
          [VariableDef "x" Nothing, VariableDef "y" Nothing] -- Inference Here
          (Just TDouble)
          (Expression [BinaryOp "+" (Identifier "x") (Identifier "y")])
    , Def "addInt"
          [VariableDef "x" Nothing, VariableDef "y" (Just TInteger)] -- Inference Here
          Nothing -- Inference Here
          (Expression [BinaryOp "+" (Identifier "x") (Identifier "y")])
    , Expressions $ Expression [BinaryOp "=" (Identifier "i") (Float 0)]
    , Expressions $ Expression
        [ BinaryOp "="
                   (Identifier "f")
                   (Call (Identifier "add") [Float 2.0, Identifier "i"])
        ]
    , Expressions $ While
        (BinaryOp "<" (Identifier "i") (Identifier "a"))
        (Expression
            [ BinaryOp "="
                       (Identifier "b")
                       (BinaryOp "+" (Identifier "i") (Int 10))
            , Call (Identifier "print") [BinaryOp "+" (Identifier "b") (Int 10)]
            ]
        )
    , Expressions
        $ Expression [Call (Identifier "add") [Identifier "a", Float 2.0]]
    , Expressions
        $ Expression [Call (Identifier "addInt") [Identifier "i", Int 3]]
    ]

allDeadCode =
    -- def test(x: int, y: int): int x + y + 1; -- no inference 
    [ Def "test"
          [VariableDef "x" (Just TInteger), VariableDef "y" (Just TInteger)]
          (Just TInteger)
          (Expression [BinaryOp "+" (Identifier "y") (Identifier "x")])
    ]

simpleFunctionDeadCode =
    -- def test(): int 10;
    -- def flex(): int 20;
    -- print(test());
    [ Def "test" [] (Just TInteger) (Expression [Int 10])
    , Def "flex" [] (Just TInteger) (Expression [Int 10])
    , Expressions $ Expression
        [Call (Identifier "print") [Call (Identifier "test") []]]
    ]

simpleFunctionDeadCode2 =
    -- def test(x, y, z, i, s, f, r): int x + y / 2;
    -- a = 2;
    -- b = 2;
    -- def test2() print(2);
    -- a = tet(123);
    [ Def
        "test"
        [ VariableDef "x" Nothing
        , VariableDef "y" Nothing
        , VariableDef "z" Nothing
        , VariableDef "i" Nothing
        , VariableDef "s" Nothing
        , VariableDef "f" Nothing
        , VariableDef "r" Nothing
        ]
        (Just TInteger)
        (Expression
            [ BinaryOp "/"
                       (BinaryOp "+" (Identifier "x") (Identifier "y"))
                       (Int 2)
            ]
        )
    , Expressions $ Expression [BinaryOp "=" (Identifier "a") (Int 2)]
    , Expressions $ Expression [BinaryOp "=" (Identifier "b") (Int 2)]
    , Def "test2" [] Nothing (Expression [Call (Identifier "print") [Int 2]])
    , Expressions $ Expression
        [BinaryOp "=" (Identifier "a") (Call (Identifier "tet") [Int 123])]
    ]

spec :: Spec
spec = do
    describe "Edge case" $ do
        it "Empty input" $ removeDeadCode [] `shouldBe` []
        it "Complete dead code" $ removeDeadCode allDeadCode `shouldBe` []
        it "Nothing to remove 1"
            $          removeDeadCode multipleValuesNoDeadCode
            `shouldBe` multipleValuesNoDeadCode
        it "Nothing to remove 2"
            $          removeDeadCode simpleFunctionNoDeadCode
            `shouldBe` simpleFunctionNoDeadCode
    describe "Basic dead code removal" $ do
        it "Remove dead code"
            $          removeDeadCode simpleFunctionDeadCode
            `shouldBe` [ Def "test" [] (Just TInteger) (Expression [Int 10])
                       , Expressions
                           $ Expression
                                 [ Call (Identifier "print")
                                        [Call (Identifier "test") []]
                                 ]
                       ]
        it "Remove dead code complexe expression"
            $          removeDeadCode simpleFunctionDeadCode2
            `shouldBe` [ Expressions
                           $ Expression [BinaryOp "=" (Identifier "a") (Int 2)]
                       , Expressions
                           $ Expression [BinaryOp "=" (Identifier "b") (Int 2)]
                       , Expressions
                           $ Expression
                                 [ BinaryOp
                                       "="
                                       (Identifier "a")
                                       (Call (Identifier "tet") [Int 123])
                                 ]
                       ]
