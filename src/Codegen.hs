module Codegen where

import AST
import State
import Data.Either
import Data.Map
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Control.Monad.Fail
import Control.Monad
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.CallingConvention as CC
import LLVM.IRBuilder.Monad (emitInstr)
import LLVM.AST.FloatingPointPredicate

newtype CodegenError = CodegenError { message :: String } deriving (Show, Eq)

type SymbolTable = [(String, Operand)]

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
  , names :: Map String Int
  , count :: Int
  , blocks :: [CodegenBlock]
  , currentBlock :: Maybe CodegenBlock
  } deriving (Show, Eq)

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState (mkName "main") [] Data.Map.empty 0 [] Nothing

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
getSymbolTable (CodegenState _ symTab _ _ _ _) = symTab

runMaybe :: Maybe a -> a
runMaybe = fromMaybe $ error "Invalid value of maybe"

fresh :: Codegen LLVM.AST.Name
fresh = do
  i <- CodegenT $ gets count
  CodegenT $ modify $ \s -> s { count = i + 1}
  return $ mkName $ show $ i + 1

freshSuggestedName :: String -> Codegen LLVM.AST.Name
freshSuggestedName sug = do
  newName <- fresh
  i <- CodegenT $ gets count
  names <- CodegenT $ gets names
  CodegenT $ modify $ \s -> s { names = Data.Map.insert sug i names }
  return newName

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
  "*"  -> mathOperation lhs rhs toMul
  "/"  -> mathOperation lhs rhs toDiv
  "+"  -> mathOperation lhs rhs toAdd
  "-"  -> mathOperation lhs rhs toSub
  "<"  -> cmpOperation  lhs rhs OLT
  ">"  -> cmpOperation  lhs rhs OGT
  "==" -> cmpOperation  lhs rhs OEQ
  "!=" -> cmpOperation  lhs rhs ONE
  "="  -> Control.Applicative.empty
  _    -> Control.Applicative.empty
generateExpr (UnaryOp name expr) = case name of
  "-"  -> Control.Applicative.empty
  "!"  -> Control.Applicative.empty
  _    -> Control.Applicative.empty
generateExpr (Identifier name) = findOperandForIdentifier name
generateExpr (AST.Call expr exprs) = callOperation expr exprs toCall
generateExpr (Primary expr) = Control.Applicative.empty

mathOperation :: KExpr -> KExpr -> (Operand -> Operand -> Instruction) -> Codegen Operand
mathOperation lhs rhs op = do
  first  <- generateExpr lhs
  second <- generateExpr rhs
  instr double $ op first second

toAdd :: Operand -> Operand -> Instruction
toAdd lhs rhs = FAdd noFastMathFlags lhs rhs []

-- Sub

toSub :: Operand -> Operand -> Instruction
toSub lhs rhs = FSub noFastMathFlags lhs rhs []

-- Mul

toMul :: Operand -> Operand -> Instruction
toMul lhs rhs = FMul noFastMathFlags lhs rhs []

-- Div

toDiv :: Operand -> Operand -> Instruction
toDiv lhs rhs = FDiv noFastMathFlags lhs rhs []

cmpOperation :: KExpr -> KExpr -> FloatingPointPredicate -> Codegen Operand
cmpOperation lhs rhs op = do
  first <- generateExpr lhs
  second <- generateExpr rhs
  instr double (FCmp op first second [])

toCall :: Operand -> [Operand] -> Instruction
toCall fn args = I.Call Nothing CC.C [] (Right fn) [] [] []

generateListExpr :: [KExpr] -> Codegen [Operand]
generateListExpr expressions = forM expressions generateExpr

callOperation :: KExpr -> [KExpr] -> (Operand -> [Operand] -> Instruction) -> Codegen Operand
callOperation fn args op = do
  fnCodegen <- generateExpr fn
  argsCodegen <- generateListExpr args
  instr double $ toCall fnCodegen argsCodegen

findOperandInSymtab :: String -> [(String, Operand)] -> Maybe Operand
findOperandInSymtab _ [] = Nothing
findOperandInSymtab value ((name, op):xs)
  | name == value = Just op
  | otherwise = findOperandInSymtab value xs

findOperandForIdentifier :: AST.Name -> Codegen Operand
findOperandForIdentifier name = do
  symbols <- CodegenT $ gets symTab
  case findOperandInSymtab name symbols of
    Just addr -> toLoadOperand addr
    Nothing -> do
      newName <- freshSuggestedName name
      addr <- toAllocaOperand
      toStoreOperand addr $ LocalReference double newName

toAllocaOperand :: Codegen Operand
toAllocaOperand = instr double $ I.Alloca double Nothing 0 []

toLoadOperand :: Operand -> Codegen Operand
toLoadOperand ptr = instr double $ I.Load Prelude.False ptr Nothing 0 []

toStoreOperand :: Operand -> Operand -> Codegen Operand
toStoreOperand ptr value = instr double $ I.Store Prelude.False ptr value Nothing 0 []

assignOperation :: KExpr -> KExpr -> Codegen Operand
assignOperation lhs rhs = do
  first <- generateExpr lhs
  second <- generateExpr rhs
  if isOperandAPointer second
  then do
    loaded <- toLoadOperand second
    toStoreOperand first loaded
  else toStoreOperand first second

isOperandAPointer :: Operand -> Bool
isOperandAPointer (LocalReference (PointerType _ _) _) = Prelude.True
isOperandAPointer (ConstantOperand _) = Prelude.False
isOperandAPointer (MetadataOperand _) = Prelude.False
