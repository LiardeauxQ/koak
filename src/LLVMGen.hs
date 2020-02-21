--{-# LANGUAGE OverloadedStrings #-}

module LLVMGen where

import           Data.String
import           Parser
import           KParser
import           AST
import           LLVM.AST
import           Codegen
import           Debug.Trace
import           LLVM.Internal.Context
import           LLVM.Internal.Module
import           State
import           Control.Applicative
import           Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)

newtype LLVMState = LLVMState{mainModule :: LLVM.AST.Module}
                      deriving (Show, Eq)

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

startKoak :: String -> IO ()
startKoak content = do
  let mod = emptyModule "test"
  case runParser koak content of
    Left e -> print e
    Right (defs, str) ->
      toLLVMCode LLVMState {mainModule = mod} $ forM defs generateDef


emptyModule :: String -> LLVM.AST.Module
emptyModule label = defaultModule { moduleName = fromString label }

addDefinitionToModule :: LLVM.AST.Module -> Definition -> LLVM.AST.Module
addDefinitionToModule module'@(LLVM.AST.Module _ _ _ _ defs) def = module' { moduleDefinitions = defs ++ [def] }

generateBlocksFromExprs :: KExprs -> [CodegenBlock]
generateBlocksFromExprs expressions =
  case runCodegenT (generateExpressions expressions) emptyCodegenState of
    Just (a, CodegenState _ _ _ _ blocks _) -> trace (show blocks) blocks
    Nothing -> trace "empty blocks" []

generateDef :: KDefs -> LLVM ()
generateDef (Def name varDefs type' expressions) = do
  let ret = trace "type" $ case type' of
                Just value -> toType value
                Nothing    -> VoidType
  let parameters = trace "param" $ convertVariablesDef varDefs
  let blocks = trace "blocks" $ map convertToBasicBlock $ generateBlocksFromExprs expressions
  mod <- trace "get module" $ LLVMT $ gets mainModule
  LLVMT $ modify $ \s -> s { mainModule = addDefinitionToModule mod $ define ret name [] blocks }
generateDef (Expressions expressions) = do
  let blocks = trace "map" $ map convertToBasicBlock $ generateBlocksFromExprs expressions
  mod <- LLVMT $ gets mainModule
  LLVMT $ modify $ \s -> s { mainModule = addDefinitionToModule mod $ define VoidType "main" [] blocks }

toLLVMCode :: LLVMState -> LLVM a -> IO ()
toLLVMCode state llvm =
  case runLLVMT llvm state of
    Just (_, LLVMState mod) ->
      withContext
        (\context ->
           withModuleFromAST context mod $ \genMod -> do
             s <- moduleLLVMAssembly genMod
             print s)
    Nothing -> empty