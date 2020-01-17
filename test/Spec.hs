
import           KoakParser
import           Parser
import           Expression
import           Test.Hspec
import           Control.Exception              ( evaluate )


runKoakParser :: String -> Either ParseError ([KoakExpression], String)
runKoakParser = runParser koak

functionDefinitionKoak = "def test(x: double): double x + 2.0;"
functionCallKoak = "test(5.0);"
assignementKoak = "y = 5;"

main :: IO ()
main = hspec $ do
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
