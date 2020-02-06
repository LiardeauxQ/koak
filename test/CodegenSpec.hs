module CodegenSpec where

import           Codegen
import           LLVM.AST.Name
import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Debug.Trace

freshCodegenState = CodegenState
  { stateName = mkName "test"
  , symTab = []
  , blocks = []
  }

runFreshCodegen :: CodegenState -> Either CodegenError (String, CodegenState)
runFreshCodegen = runCodegen fresh

getStringValue :: Either CodegenError (String, CodegenState) -> String
getStringValue value = case value of
  Left e -> "Error"
  Right (s, _) -> s

spec :: Spec
spec =
  describe "Codegen utilisation" $ do
    it "Create fresh name" $ do
      let value = runFreshCodegen freshCodegenState
      getStringValue value `shouldBe` "1"
    it "Create fresh name" $ do
      let value = runFreshCodegen freshCodegenState
      getStringValue value `shouldBe` "1"