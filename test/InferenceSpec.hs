module InferenceSpec where
import           TypeInference
import           AST
import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Debug.Trace

simpleFunction = -- def test(x) x + 2.0;
    [ Def "test"
          [VariableDef "x" Nothing]
          Nothing
          (Expression [BinaryOp "+" (Identifier "x") (Float 2.0)])
    ]

simpleFunction2 = -- def test(x, y: int) x + y;
    [ Def "test"
          [VariableDef "x" Nothing, VariableDef "y" (Just TInteger)]
          Nothing
          (Expression [BinaryOp "+" (Identifier "y") (Identifier "x")])
    ]

simpleFunction3 = -- def test(x, y, z: int) x + y + 2.0;
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
    ]

multipleValues = -- 
    -- a = 50;
    -- def add(x, y): double x + y;
    -- i = 0.0;
    -- f = add(2.0, i);
    -- while i < a do
    --     b = i + 10 :
    --     print(b + 10);
    -- print(a);
    -- print(i);
    --
    [ Expressions $ Expression [BinaryOp "=" (Identifier "a") (Int 50)]
    , Def "add"
          [VariableDef "x" Nothing, VariableDef "y" Nothing] -- Inference Here
          (Just TDouble)
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
    , Expressions $ Expression [Call (Identifier "print") [Identifier "a"]]
    , Expressions $ Expression [Call (Identifier "print") [Identifier "i"]]
    ]

noInferenceFunction = -- def test(x: int, y: int): int x + y + 1; -- no inference 
    [ Def "test"
          [VariableDef "x" (Just TInteger), VariableDef "y" (Just TInteger)]
          (Just TInteger)
          (Expression [BinaryOp "+" (Identifier "y") (Identifier "x")])
    ]


spec :: Spec
spec = do
    describe "Edge case" $ do
        it "Empty tree" $ infereType [] `shouldBe` Right []
        it "No inference"
            $          infereType noInferenceFunction
            `shouldBe` Right noInferenceFunction
    describe "Function inference" $ do
        it "Basic function inference 0"
            $          infereType simpleFunction
            `shouldBe` Right
                           [ Def
                                 "test"
                                 [VariableDef "x" (Just TDouble)]
                                 (Just TDouble)
                                 (Expression
                                     [BinaryOp "+" (Identifier "x") (Float 2.0)]
                                 )
                           ]
        it "Basic function inference 1"
            $          infereType simpleFunction2
            `shouldBe` Right
                           [ Def
                                 "test"
                                 [ VariableDef "x" (Just TInteger)
                                 , VariableDef "y" (Just TInteger)
                                 ]
                                 (Just TInteger)
                                 (Expression
                                     [ BinaryOp "+"
                                                (Identifier "y")
                                                (Identifier "x")
                                     ]
                                 )
                           ]
        it "Basic function inference 2"
            $          infereType simpleFunction3
            `shouldBe` Right
                           [ Def
                                 "test"
                                 [ VariableDef "x" (Just TDouble)
                                 , VariableDef "y" (Just TDouble)
                                 , VariableDef "z" (Just TInteger)
                                 ]
                                 (Just TDouble)
                                 (Expression
                                     [ BinaryOp
                                           "+"
                                           (Float 2.0)
                                           (BinaryOp "+"
                                                     (Identifier "x")
                                                     (Identifier "y")
                                           )
                                     ]
                                 )
                           ]
