module Codegen where

import AST
import State
import Data.Either
import Data.Map
import Control.Monad.Fail
import Control.Monad
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.IRBuilder.Monad as B
import LLVM.IRBuilder.Monad (emitInstr)
import LLVM.AST.FloatingPointPredicate
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)

newtype CodegenError = CodegenError { message :: String } deriving (Show, Eq)

type SymbolTable = Map String Operand

data CodegenBlock = CodegenBlock
  { blockName :: LLVM.AST.Name
  , instructions :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  } deriving (Show, Eq)

initNewBlock :: String -> CodegenBlock
initNewBlock name = CodegenBlock
  { blockName = mkName name
  , instructions = []
  , term = Nothing
  }

getBlockInstructions :: CodegenBlock -> [Named Instruction]
getBlockInstructions (CodegenBlock _ instructions _) = instructions

addBlockInstruction :: CodegenBlock -> Named Instruction -> CodegenBlock
addBlockInstruction (CodegenBlock name instructions term) new = CodegenBlock
  { blockName = name
  , instructions = instructions ++ [new]
  , term = term
  }

modifyBlockTerm :: CodegenBlock -> Named Terminator -> CodegenBlock
modifyBlockTerm (CodegenBlock name instructions _) newTerm = CodegenBlock
  { blockName = name
  , instructions = instructions
  , term = Just newTerm
  }

data CodegenState = CodegenState
  { stateName   :: LLVM.AST.Name
  , symTab :: SymbolTable
  , count :: Word
  , blocks :: [CodegenBlock]
  , currentBlock :: Maybe CodegenBlock
  } deriving (Show, Eq)

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState (mkName "main") Data.Map.empty 0 [] Nothing

newtype CodegenT m a = CodegenT { unCodegenT :: StateT CodegenState m a }

type Codegen = CodegenT Maybe

instance (Monad m) => Functor (CodegenT m) where
  fmap f p = CodegenT $ f <$> unCodegenT p

instance (Monad m) => Applicative (CodegenT m) where
  pure a = CodegenT $ pure a
  f <*> a = CodegenT $ unCodegenT f <*> unCodegenT a

instance (Monad m) => Monad (CodegenT m) where
  return a = CodegenT $ return a
  a >>= f = CodegenT $ StateT $ \e -> do
    (a', e') <- runCodegenT a e
    runCodegenT (f a') e'

instance (Alternative m, Monad m) => Alternative (CodegenT m) where
  empty = CodegenT Control.Applicative.empty
  (CodegenT a) <|> (CodegenT b) = CodegenT $ a <|> b

runCodegenT :: CodegenT m a -> CodegenState -> m (a, CodegenState)
runCodegenT = runState . unCodegenT

getSymbolTable :: CodegenState -> SymbolTable
getSymbolTable (CodegenState _ symTab _ _ _) = symTab

runMaybe :: Maybe a -> a
runMaybe = fromMaybe (error "Invalid value of maybe")

fresh :: Codegen LLVM.AST.Name
fresh = do
  i <- CodegenT $ gets count
  CodegenT $ modify $ \s -> s { count = i + 1}
  return $ mkName $ show $ i + 1

instr :: Type -> Instruction -> Codegen Operand
instr ret i = do
  current <- CodegenT $ gets currentBlock
  name <- fresh
  case current of
    Just block ->
      CodegenT $ modify $ \s -> s { currentBlock = Just $ addBlockInstruction block (name := i)}
    Nothing -> error "Nothing"
  return $ LocalReference ret name

define :: Type -> String -> [(String, Type)] -> [LLVM.AST.BasicBlock] -> Definition
define ret name parametersAttr blocks = GlobalDefinition $ functionDefaults {
  returnType = ret,
  name = mkName name,
  parameters = (toParameters parametersAttr, Prelude.False),
  basicBlocks = blocks
}

toParameters :: [(String, Type)] -> [Parameter]
toParameters elems = [Parameter type' (mkName name) [] | (name, type') <- elems]

convertVariablesDef :: [VariableDef] -> [(String, Type)]
convertVariablesDef defs = [(name, toType type') | (VariableDef name (Just type')) <- defs]

toType :: KType -> Type
toType TInteger = FloatingPointType DoubleFP
toType TDouble = FloatingPointType DoubleFP
toType TVoid = VoidType

double = FloatingPointType DoubleFP

intSize = 4

generateExpr :: KExpr -> Codegen Operand
generateExpr (Int value) = return $ ConstantOperand $ C.Int intSize value
generateExpr (Float value) = return $ ConstantOperand $ C.Float $ F.Double value
generateExpr (BinaryOp name lhs rhs) = case name of
  "*"  -> mathOperationInstruction lhs rhs toMulInstruction
  "/"  -> mathOperationInstruction lhs rhs toDivInstruction
  "+"  -> mathOperationInstruction lhs rhs toAddInstruction
  "-"  -> mathOperationInstruction lhs rhs toSubInstruction
  "<"  -> cmpOperationInstruction  lhs rhs OLT
  ">"  -> cmpOperationInstruction  lhs rhs OGT
  "==" -> cmpOperationInstruction  lhs rhs OEQ
  "!=" -> cmpOperationInstruction  lhs rhs ONE
  "="  -> Control.Applicative.empty
  _    -> Control.Applicative.empty
generateExpr (UnaryOp name expr) = Control.Applicative.empty
generateExpr (Identifier name) = return $ LocalReference double (mkName name)
generateExpr (AST.Call expr exprs) = Control.Applicative.empty
generateExpr (Primary expr) = Control.Applicative.empty

mathOperationInstruction :: KExpr -> KExpr -> (Operand -> Operand -> Instruction) -> Codegen Operand
mathOperationInstruction lhs rhs op = do
  first  <- generateExpr lhs
  second <- generateExpr rhs
  instr double $ op first second

toAddInstruction :: Operand -> Operand -> Instruction
toAddInstruction lhs rhs = FAdd noFastMathFlags lhs rhs []

-- Sub

toSubInstruction :: Operand -> Operand -> Instruction
toSubInstruction lhs rhs = FSub noFastMathFlags lhs rhs []

-- Mul

toMulInstruction :: Operand -> Operand -> Instruction
toMulInstruction lhs rhs = FMul noFastMathFlags lhs rhs []

-- Div

toDivInstruction :: Operand -> Operand -> Instruction
toDivInstruction lhs rhs = FDiv noFastMathFlags lhs rhs []

cmpOperationInstruction :: KExpr -> KExpr -> FloatingPointPredicate -> Codegen Operand
cmpOperationInstruction lhs rhs op = do
  first <- generateExpr lhs
  second <- generateExpr rhs
  instr double (FCmp op first second [])