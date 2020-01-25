module Codegen where

import AST
import State
import Data.Either
import Control.Monad
import Control.Monad.Fail
import Control.Applicative

import LLVM.AST

newtype CodegenError = CodegenError{message :: String} deriving (Show, Eq)

newtype Codegen a = Codegen { unCodegen :: StateT [KDefs] (Either CodegenError) a }

instance Functor Codegen where
  fmap f p = Codegen $ f <$> unCodegen p

instance Applicative Codegen where
  pure a = Codegen $ pure a
  f <*> a = Codegen $ unCodegen f <*> unCodegen a

instance Monad Codegen where
  return a = Codegen $ return a
  a >>= f = Codegen $ StateT $ \e -> do
    (a', e') <- runCodegen a e
    runCodegen (f a') e'

instance Alternative Codegen where
  empty = Codegen $ StateT $ \e ->
    Left $ CodegenError { message = "Empty" }
  a <|> b = Codegen $ StateT $ \e -> case (runCodegen a e, runCodegen b e) of
    (Left x, Left y) -> Left x
    (Right x, Left y) -> Right x
    (Left x, Right y) -> Right y
    (Right x, Right y) -> Right x

instance MonadFail Codegen where
  fail s = Codegen $ StateT $ \e ->
    Left $ CodegenError { message = s }

runCodegen :: Codegen a -> [KDefs] -> Either CodegenError (a, [KDefs])
runCodegen = runState . unCodegen

generateCode :: [KDefs] -> Either CodegenError (Codegen String)
generateCode (Def {}:xs) = Left $ CodegenError "Def"
generateCode (Expressions {}: xs) = Left $ CodegenError "Expressions"

generateNewVar :: VariableDef -> Either CodegenError (Codegen String)
generateNewVar (VariableDef name terminator) = Left $ CodegenError "Variable"

generateNewExpr :: KExpr -> Either CodegenError (Codegen String)
generateNewExpr (Int value) = Left $ CodegenError "Int"
generateNewExpr (Float value) = Left $ CodegenError "Double"
generateNewExpr (BinaryOp name left right) = Left $ CodegenError "Binary op"
generateNewExpr (UnaryOp name value) = Left $ CodegenError "Unary op"
generateNewExpr (Identifier name) = Left $ CodegenError "Identifier"
generateNewExpr (AST.Call value values) = Left $ CodegenError "Call"
generateNewExpr (Primary values) = Left $ CodegenError "Primary"

generateNewExprs :: KExprs -> Either CodegenError (Codegen String)
generateNewExprs (For v1 v2 v3 v4 v5 v6) = Left $ CodegenError "For"
generateNewExprs (If v1 v2 v3) = Left $ CodegenError "If"
generateNewExprs (While v1 v2) = Left $ CodegenError "While"
generateNewExprs (Expression values) = Left $ CodegenError "Expression"
