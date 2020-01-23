
import           KoakParser
import           Parser
import           Expression
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
    \    if (x < 3) then\n\
    \        1\n\
    \    else\n\
    \        fib(x-1)+fib(x-2);\n"

main :: IO ()
main = hspec $ describe "Function definition." $ do
    it "Parse a function definition." $ do
        let value = runKoakParser functionDefinitionKoak
        value `shouldBe` Right
            ( [ Def "test"
                    [VariableDef "x" (Just TDouble)]
                    (Just TDouble)
                    (Expression [BinaryOp "+" (Identifier "x") (Float 2.0)])
              ]
            , ""
            )

    it "Parse a function call." $ do
        let value = runKoakParser functionCallKoak
        value
            `shouldBe` Right
                           ( [ Expressions
                                   (Expression
                                       [Call (Identifier "test") [Float 5.0]]
                                   )
                             ]
                           , ""
                           )

    it "Parse an assignement of an int." $ do
        let value = runKoakParser assignementKoak
        value
            `shouldBe` Right
                           ( [ Expressions
                                   (Expression
                                       [BinaryOp "=" (Identifier "y") (Int 5)]
                                   )
                             ]
                           , ""
                           )

    it "Parse complexe function definition koak." $ do
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
