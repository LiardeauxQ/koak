{-# LANGUAGE BlockArguments #-}

module LLVMGen where

import           Data.String
import           Parser
import           KParser
import           AST
import           DeadCode
import           LLVM.AST
import           Codegen
import           Debug.Trace
import           LLVM.Internal.Context
import           LLVM.Internal.Module
import qualified LLVM.ExecutionEngine as EE
import           State
import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Maybe (fromMaybe)
import           Foreign.Ptr ( FunPtr, castFunPtr )

data LLVMState = LLVMState
  { mainModule :: LLVM.AST.Module
  , stackBlocks :: [BasicBlock]
  , codegenState :: CodegenState
  } deriving (Show, Eq)

newtype LLVMT m a = LLVMT { unLLVMT :: StateT LLVMState m a }

type LLVM = LLVMT Maybe

instance (Monad m) => Functor (LLVMT m) where
  fmap f p = LLVMT $ f <$> unLLVMT p

instance (Monad m) => Applicative (LLVMT m) where
  pure a = LLVMT $ pure a
  f <*> a = LLVMT $ unLLVMT f <*> unLLVMT a

instance (Monad m) => Monad (LLVMT m) where
  return a = LLVMT $ return a
  a >>= f = LLVMT $ StateT $ \e -> do
    (a', e') <- runLLVMT a e
    runLLVMT (f a') e'

instance (Alternative m, Monad m) => Alternative (LLVMT m) where
  empty = LLVMT Control.Applicative.empty
  (LLVMT a) <|> (LLVMT b) = LLVMT $ a <|> b


runLLVMT :: Monad m => LLVMT m a -> LLVMState -> m (a, LLVMState)
runLLVMT = runState . unLLVMT


emptyModule :: String -> LLVM.AST.Module
emptyModule label = defaultModule { moduleName = fromString label }


addDefinitionToModule :: LLVM.AST.Module -> Definition -> LLVM.AST.Module
addDefinitionToModule module'@(LLVM.AST.Module _ _ _ _ defs) def = module' { moduleDefinitions = defs ++ [def] }


replaceAt :: Int -> a -> [a] -> [a]
replaceAt n value all@(x:xs)
  | n == 0 = value : xs
  | otherwise = x : replaceAt (n - 1) value xs


mergeTwoBlocks :: BasicBlock -> BasicBlock -> BasicBlock
mergeTwoBlocks (BasicBlock firstName firstInst _) (BasicBlock _ secondInst secondTerm) =
  BasicBlock firstName (firstInst ++ secondInst) secondTerm

getBasicBlockName :: BasicBlock -> LLVM.AST.Name
getBasicBlockName (BasicBlock name _ _) = name


mergeTwoBlocksIf :: BasicBlock -> BasicBlock -> (BasicBlock -> BasicBlock -> Bool) -> Maybe BasicBlock
mergeTwoBlocksIf first second condition
  | condition first second = Just $ mergeTwoBlocks first second
  | otherwise = Nothing


blockHasSameName :: BasicBlock -> BasicBlock -> Bool
blockHasSameName (BasicBlock firstName _ _) (BasicBlock secondName _ _) = firstName == secondName

isEntryBlock :: BasicBlock -> BasicBlock -> Bool
isEntryBlock _ (BasicBlock name _ _) = mkName "entry" == name

mergeWithLastBlock :: BasicBlock -> [BasicBlock] -> [BasicBlock]
mergeWithLastBlock new []     = [new]
mergeWithLastBlock new blocks =
  case mergedBlock of
    Just value -> replaceAt (length blocks - 1) value blocks
    Nothing -> blocks ++ [new]
  where mergedBlock = mergeTwoBlocksIf (last blocks) new isEntryBlock


toBasicBlocks :: [CodegenBlock] -> [BasicBlock] -> [BasicBlock]
toBasicBlocks [] blocks = blocks
toBasicBlocks (c:cs) basBlocks = toBasicBlocks cs $ mergeWithLastBlock convertedBlock basBlocks
  where convertedBlock = convertToBasicBlock c


generateBlocksFromExprs :: KExprs -> LLVM ()
generateBlocksFromExprs expressions = do
  state <- LLVMT $ gets codegenState
  basicBlocks <- LLVMT $ gets stackBlocks
  case runCodegenT result state of
    Just (a, all@(CodegenState _ _ _ _ blocks current)) -> do
      case current of
        Just value -> LLVMT $ modify $ \s -> s { stackBlocks = toBasicBlocks (blocks ++ [value]) basicBlocks }
        Nothing -> LLVMT $ modify $ \s -> s { stackBlocks = toBasicBlocks blocks basicBlocks }
      LLVMT $ modify $ \s -> s { codegenState = cleanCodegenState all }
    Nothing -> return ()
  where
    result = generateExpressions expressions

generateFunc :: KExprs -> [(String, Type)] -> Codegen ()
generateFunc body parameters = do
  CodegenT $ modify $ \s -> s { currentBlock = Just $ initNewBlock "entry" }
  forM parameters \(name, type') -> do
    addr <- addToSymtab $ name
    toStore addr $ LocalReference double $ mkName name
    return ()
  generateExpressions body
  return ()

generateDef :: KDefs -> LLVM ()
generateDef (Def name varDefs type' body) = do
  let ret = case type' of
                Just value -> toType value
                Nothing    -> VoidType
  let parameters = convertVariablesDef varDefs

  mod <- LLVMT $ gets mainModule
  case runCodegenT (generateFunc body parameters) emptyCodegenState of
      Just (a, all@(CodegenState _ _ _ _ blocks current)) -> do
        let basicBlocks = case current of
                      Just value -> toBasicBlocks (blocks ++ [value]) []
                      Nothing -> toBasicBlocks blocks []
        LLVMT $ modify \s -> s { mainModule = addDefinitionToModule mod $ define ret name parameters basicBlocks }
        return ()
      Nothing -> return ()
generateDef (Expressions expressions) = generateBlocksFromExprs expressions


fillModule :: LLVM.AST.Module -> [BasicBlock] -> LLVM.AST.Module
fillModule mod blocks =
  if null blocks
    then mod
    else addDefinitionToModule mod $ define double "main" [] blocks


jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing


foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))


startKoak :: String -> String -> IO ()
startKoak content filename = do
  let mod = emptyModule "test"
  case runParser koak content of
    Left e -> print e
    Right (defs, str) ->
      toLLVMCode (File filename) LLVMState
      { mainModule = mod
      , stackBlocks = []
      , codegenState = emptyCodegenState
      } $ forM (removeDeadCode  defs) generateDef


toLLVMCode :: File -> LLVMState -> LLVM a -> IO ()
toLLVMCode file state llvm =
  case runLLVMT llvm state of
    Just (_, LLVMState mod blocks _) -> do
      let updatedMod = fillModule mod blocks
      withContext
        (\context -> jit context $ \executionEngine ->
           withModuleFromAST context updatedMod $ \genMod -> do
             writeLLVMAssemblyToFile file genMod
             EE.withModuleInEngine executionEngine genMod $ \ee -> do
              mainfn <- EE.getFunction ee (mkName "main")
              case mainfn of
                Just fn -> do
                  res <- run fn
                  putStrLn $ "Result: " ++ show res
                Nothing -> return ()
        )
    Nothing -> empty
