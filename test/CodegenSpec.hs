module CodegenSpec where

import           Codegen
import           Data.Map
import           LLVM.AST.Name
import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Debug.Trace

freshCodegenState = CodegenState
  { stateName = mkName "test"
  , symTab    = Data.Map.empty
  , count     = 0
  , blocks    = []
  , currentBlock = Nothing
  }

runFreshCodegen :: CodegenState -> Maybe (Name, CodegenState)
runFreshCodegen = runCodegenT fresh

getStringValue :: Maybe (Name, CodegenState) -> Name
getStringValue value = case value of
  Nothing -> mkName "Error"
  Just (s, _) -> s

spec :: Spec
spec =
  describe "Codegen utilisation" $ do
    it "Create fresh name" $ do
      let value = runFreshCodegen freshCodegenState
      getStringValue value `shouldBe` mkName "1"
    it "Create fresh name" $ do
      let value = runFreshCodegen freshCodegenState
      getStringValue value `shouldBe` mkName "1"