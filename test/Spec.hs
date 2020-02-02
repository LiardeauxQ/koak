
import           KParser
import           Parser
import           AST
import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Debug.Trace

runKoakParser :: String -> Either ParseError ([KDefs], String)
runKoakParser = runParser koak

functionDefinitionKoak = "def test(x: double): double x + 2.0;"
functionCallKoak = "test(5.0);"
assignementKoak = "y = 5;"
complexeFunctionDefinitionKoak =
    "def fibi(x)\n\
    \    var a = 1\n\
    \    var b = 1\n\
    \    var c = 0\n\
    \    (for i = 3, i < x, 1.0 in\n\
    \      c = (a + b) :\n\
    \      a = b :\n\
    \      b = c) :\n\
    \    b;\n"
functionWithIf =
    "def fib(x)\n\
    \    if x < 3 then\n\
    \        1\n\
    \    else\n\
    \        fib(x-1)+fib(x-2);"
functionWithComplexeFor =
    "def mandelhelp(xmin xmax xstep ymin ymax ystep)\n\
    \      for y = ymin, y < ymax, ystep in (\n\
    \          (for x = xmin, x < xmax, xstep in\n\
    \              printdensity(mandelconverge(x,y)))\n\
    \          : putchard(10))\n"

main :: IO ()
main = hspec $ do

    describe "Parse if Expr" $ it "Function with complexe for." $ do
        let value = runParser ifExpr "if x < 2 then 2 else 3"
        value `shouldBe` Right
            ( If (BinaryOp "<" (Identifier "x") (Int 2))
                 (Expression [Int 2])
                 (Just (Expression [Int 3]))
            , ""
            )

    describe "Function definition." $ do
        it "Function definition." $ do
            let value = runKoakParser functionDefinitionKoak
            value `shouldBe` Right
                ( [ Def
                        "test"
                        [VariableDef "x" (Just TDouble)]
                        (Just TDouble)
                        (Expression [BinaryOp "+" (Identifier "x") (Float 2.0)])
                  ]
                , ""
                )

        it "Function call." $ do
            let value = runKoakParser functionCallKoak
            value
                `shouldBe` Right
                               ( [ Expressions
                                       (Expression
                                           [ Call (Identifier "test")
                                                  [Float 5.0]
                                           ]
                                       )
                                 ]
                               , ""
                               )

        it "Assignement of an int." $ do
            let value = runKoakParser assignementKoak
            value
                `shouldBe` Right
                               ( [ Expressions
                                       (Expression
                                           [ BinaryOp "="
                                                      (Identifier "y")
                                                      (Int 5)
                                           ]
                                       )
                                 ]
                               , ""
                               )

        it "Complexe function definition koak." $ do
            let value = runKoakParser complexeFunctionDefinitionKoak
            value `shouldBe` Right
                ( [ Def
                        "fibi"
                        [VariableDef "x" Nothing]
                        Nothing
                        (Expression
                            [ BinaryOp "=" (Identifier "a") (Int 1)
                            , BinaryOp "=" (Identifier "b") (Int 1)
                            , BinaryOp "=" (Identifier "c") (Int 0)
                            , Primary
                                (For
                                    (Identifier "i")
                                    (Int 3)
                                    (Identifier "i")
                                    (Identifier "x")
                                    (Float 1.0)
                                    (Expression
                                        [ BinaryOp
                                            "="
                                            (Identifier "c")
                                            (BinaryOp "+"
                                                      (Identifier "a")
                                                      (Identifier "b")
                                            )
                                        , BinaryOp "="
                                                   (Identifier "a")
                                                   (Identifier "b")
                                        , BinaryOp "="
                                                   (Identifier "b")
                                                   (Identifier "c")
                                        ]
                                    )
                                )
                            , Identifier "b"
                            ]
                        )
                  ]
                , ""
                )

        it "Function with if." $ do
            let value = runKoakParser functionWithIf
            value `shouldBe` Right
                ( [ Def
                        "fib"
                        [VariableDef "x" Nothing]
                        Nothing
                        (If
                            (BinaryOp "<" (Identifier "x") (Int 3))
                            (Expression [Int 1])
                            (Just
                                (Expression
                                    [ BinaryOp
                                          "+"
                                          (Call
                                              (Identifier "fib")
                                              [ BinaryOp "-"
                                                         (Identifier "x")
                                                         (Int 1)
                                              ]
                                          )
                                          (Call
                                              (Identifier "fib")
                                              [ BinaryOp "-"
                                                         (Identifier "x")
                                                         (Int 2)
                                              ]
                                          )
                                    ]
                                )
                            )
                        )
                  ]
                , ""
                )

        it "Function with complexe for." $ do
            let value = runKoakParser functionWithComplexeFor
            value `shouldBe` Right
                ( [ Def
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
                                                  [ Call
                                                        (Identifier
                                                            "mandelconverge"
                                                        )
                                                        [ Identifier "x"
                                                        , Identifier "y"
                                                        ]
                                                  ]
                                              , Call (Identifier "putchar")
                                                     [Int 10]
                                              ]
                                          )
                                      )
                                ]
                            )
                        )
                  ]
                , ""
                )

