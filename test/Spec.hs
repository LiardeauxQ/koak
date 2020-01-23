
import           KoakParser
import           Parser
import           Expression
import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Debug.Trace

runKoakParser :: String -> Either ParseError ([KoakExpression], String)
runKoakParser = runParser koak

functionDefinitionKoak = "def test(x: double): double x + 2.0;"
functionCallKoak = "test(5.0);"
assignementKoak = "y = 5;"
complexeFunctionDefinitionKoak =
    "def fibi(x)\n\
        \var a = 1, b = 1, c = 0 in\n\
        \(for i = 3, i < x, 1.0 in\n\
        \  c = (a + b) :\n\
        \  a = b :\n\
        \  b = c) :\n\
        \b;\n"

main :: IO ()
main = trace complexeFunctionDefinitionKoak >> hspec $ do
    describe "Function definition." $ do
        it "Parse a function definition." $ do
            let value = runKoakParser functionDefinitionKoak
            value `shouldBe` Right
                ( [ (Def
                        (DefineFunction
                            "test"
                            [Variable "x" (Just Double)]
                            (Just Double)
                            (Exprs [(BinaryOp "+" (Var "x") (Float 2.0))])
                        )
                    )
                  ]
                , ""
                )



        it "Parse a function call." $ do
            let value = runKoakParser functionCallKoak
            value `shouldBe` Right
                ([Expr (Exprs [(Call "test" [Float 5.0])])], "")

        it "Parse an assignement of an int." $ do
            let value = runKoakParser assignementKoak
            value `shouldBe` Right
                ([Expr (Exprs [(BinaryOp "=" (Var "y") (Int 5))])], "")

        it "Parse complexe function definition koak." $ do
            let value = runKoakParser complexeFunctionDefinitionKoak
            value `shouldBe` Right
                ( [ (Def
                        (DefineFunction
                            "fibi"
                            [(Variable "x" Nothing)]
                            Nothing
                            (Exprs
                                [ (BinaryOp "=" (Var "a") (Int 1))
                                , (BinaryOp "=" (Var "b") (Int 1))
                                , (BinaryOp "=" (Var "c") (Int 0))
                                , (For
                                      "i"
                                      (Int 3)
                                      (BinaryOp "<" (Var "i") (Int 3))
                                      (Float 1.0)
                                      (Exprs
                                          [ (BinaryOp
                                                "="
                                                (Var "c")
                                                (BinaryOp "+"
                                                          (Var "a")
                                                          (Var "b")
                                                )
                                            )
                                          , (BinaryOp "=" (Var "a") (Var "b"))
                                          , (BinaryOp "=" (Var "b") (Var "c"))
                                          ]
                                      )
                                  )
                                ]
                            )
                        )
                    )
                  ]
                , ""
                )



