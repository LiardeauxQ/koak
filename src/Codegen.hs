{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
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
import Debug.Trace

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Typed
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import LLVM.AST.AddrSpace
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

createNextBlock :: String -> CodegenState -> CodegenState
createNextBlock blockName (CodegenState name tab names count blocks current) = CodegenState
  { stateName = name
  , symTab = tab
  , names = names
  , count = count
  , blocks = case current of
    Just value -> blocks ++ [value]
    Nothing -> blocks
  , currentBlock = Just $ initNewBlock blockName }


getCurrentBlockName :: CodegenBlock -> LLVM.AST.Name
getCurrentBlockName (CodegenBlock name _ _) = name

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


endCurrentBlock :: CodegenState -> CodegenState
endCurrentBlock (CodegenState name tab names count blocks currentBlock) = CodegenState
  { stateName = name
  , symTab = tab
  , names = names
  , count = count
  , blocks = case currentBlock of
    Just value -> blocks ++ [value]
    Nothing -> blocks
  , currentBlock = Nothing
  }

convertToBasicBlock :: CodegenBlock -> BasicBlock
convertToBasicBlock (CodegenBlock name instructions term) = case term of
  Just value -> BasicBlock name instructions value
  Nothing -> error "No term expression"

data CodegenState = CodegenState
  { stateName   :: LLVM.AST.Name
  , symTab :: SymbolTable
  , names :: Map String Word
  , count :: Word
  , blocks :: [CodegenBlock]
  , currentBlock :: Maybe CodegenBlock
  } deriving (Show, Eq)

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState (mkName "main") [] Data.Map.empty 0 [] Nothing

cleanCodegenState :: CodegenState -> CodegenState
cleanCodegenState (CodegenState name symtab names count _ _) = CodegenState
  { stateName = name
  , symTab = symtab
  , names = names
  , count = count
  , blocks = []
  , currentBlock = Nothing
  }

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

runCodegenT :: Monad m => CodegenT m a -> CodegenState -> m (a, CodegenState)
runCodegenT = runState . unCodegenT

getSymbolTable :: CodegenState -> SymbolTable
getSymbolTable (CodegenState _ symTab _ _ _ _) = symTab

getLastSymbolTableOperand :: SymbolTable -> Operand
getLastSymbolTableOperand symtab = operand
  where (name, operand) = last symtab

runMaybe :: Maybe a -> a
runMaybe = fromMaybe $ error "Invalid value of maybe"

fresh :: Codegen Word
fresh = do
  i <- CodegenT $ gets count
  CodegenT $ modify $ \s -> s { count = i + 1}
  return $ i + 1

freshSuggestedName :: String -> Codegen LLVM.AST.Name
freshSuggestedName sug = do
  newName <- fresh
  i <- CodegenT $ gets count
  names <- CodegenT $ gets names
  CodegenT $ modify $ \s -> s { names = Data.Map.insert sug i names }
  return $ UnName newName

instr :: Type -> Instruction -> Codegen Operand
instr ret i = do
  current <- CodegenT $ gets currentBlock
  ref <- fresh
  let name = UnName ref
  case current of
    Just block -> do
      case ret of
        VoidType ->
          CodegenT $ modify $ \s -> s { currentBlock = Just $ addBlockInstruction block (Do i)}
        _ ->
          CodegenT $ modify $ \s -> s { currentBlock = Just $ addBlockInstruction block (name := i)}
      return $ LocalReference ret name
    Nothing -> error "Cannot create block for instruction"

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator value = do
  current <- CodegenT $ gets currentBlock
  case current of
    Just block ->
      CodegenT $ modify $ \s -> s { currentBlock = Just $ block { term = Just value }}
    Nothing -> error "Cannot create block for terminator"
  return value


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

variableDefToOperand :: VariableDef -> Codegen Operand
variableDefToOperand (VariableDef name (Just type')) = do
  symTab <- CodegenT $ gets symTab
  ptr <- toAlloca
  toStore ptr $ LocalReference double (mkName name)
  CodegenT $ modify $ \s -> s {symTab = symTab ++ [(name, ptr)]}
  return ptr

generateSymbolTable :: [VariableDef] -> Codegen [Operand]
generateSymbolTable defs = forM defs variableDefToOperand

toType :: KType -> Type
toType TInteger = FloatingPointType DoubleFP
toType TDouble = FloatingPointType DoubleFP
toType TVoid = VoidType

double = FloatingPointType DoubleFP

intSize = 4
zero = ConstantOperand $ C.Float $ F.Double 0.0
one = ConstantOperand $ C.Float $ F.Double 1.0


handleOptionalElse :: Maybe KExprs -> Codegen (Maybe Operand)
handleOptionalElse exprs = case exprs of
  Just value -> do
    CodegenT $ modify $ \s -> createNextBlock "else.block" s
    op <- generateExpressions value
    toBr "if.block"
    return $ Just op
  Nothing -> return Nothing


generateExpressions :: KExprs -> Codegen Operand
generateExpressions (For expr1 expr2 expr3 expr4 expr5 exprs) = do
  current <- CodegenT $ gets currentBlock
  CodegenT $ modify $ \s -> s {currentBlock = Just $ fromMaybe (initNewBlock "entry") current}
  counter <- generateExpression expr1
  initialValue <- generateExpression expr2
  toStore counter initialValue
  toBr "for.block"

  CodegenT $ modify $ \s -> createNextBlock "for.block" s
  generateExpressions exprs
  incValue <- generateExpression expr5
  loadedCounter <- toLoad counter

  nextVar <- instr double $ toAdd loadedCounter incValue

  toStore counter nextVar

  cmp <- generateExpression $ BinaryOp "<" expr3 expr4
  toCondBr cmp "for.block" "afterfor.block"

  CodegenT $ modify $ \s -> createNextBlock "afterfor.block" s
  toRet zero
  return zero
generateExpressions (If expr exprs1 exprs2) = do
  current <- CodegenT $ gets currentBlock
  CodegenT $ modify $ \s -> s {currentBlock = Just $ fromMaybe (initNewBlock "entry") current}
  cond <- generateExpression expr

  toCondBr cond "then.block" "else.block"
  CodegenT $ modify $ \s -> createNextBlock "then.block" s
  thenOp <- generateExpressions exprs1

  toBr "if.block"
  optionalElse <- handleOptionalElse exprs2
  CodegenT $ modify $ \s -> createNextBlock "if.block" s
  res <- case optionalElse of
          Just value -> toPhi double [(thenOp, mkName "then.block"), (value, mkName "else.block")]
          Nothing -> toPhi double [(thenOp, mkName "then.block")]
  toRet res
  return res
generateExpressions (While expr exprs) = do
  current <- CodegenT $ gets currentBlock
  CodegenT $ modify $ \s -> s {currentBlock = Just $ fromMaybe (initNewBlock "entry") current}
  toBr "while.block"

  CodegenT $ modify $ \s -> createNextBlock "while.block" s
  generateExpressions exprs
  cmp <- generateExpression expr
  toCondBr cmp "while.block" "afterwhile.block"

  CodegenT $ modify $ \s -> createNextBlock "afterwhile.block" s
  toRet zero
  return zero
generateExpressions (Expression exprs) = do
  current <- CodegenT $ gets currentBlock
  CodegenT $ modify $ \s -> s {currentBlock = Just $ fromMaybe (initNewBlock "entry") current}
  retVal <- forM exprs generateExpression
  symtab <- CodegenT $ gets symTab
  toRet $ last retVal
  return $ last retVal

generateExpression :: KExpr -> Codegen Operand
generateExpression (Int value) = toNumOperand $ fromInteger value
generateExpression (Float value) = toNumOperand value
generateExpression (BinaryOp name lhs rhs) = case name of
  "*"  -> mathOperation lhs rhs toMul
  "/"  -> mathOperation lhs rhs toDiv
  "+"  -> mathOperation lhs rhs toAdd
  "-"  -> mathOperation lhs rhs toSub
  "<"  -> cmpOperation  lhs rhs OLT
  ">"  -> cmpOperation  lhs rhs OGT
  "==" -> cmpOperation  lhs rhs OEQ
  "!=" -> cmpOperation  lhs rhs ONE
  "="  -> assignOperation lhs rhs
  _    -> trace "Error with BinaryOp" Control.Applicative.empty
generateExpression (UnaryOp name expr) = case name of
  "-"  -> trace "Error -" Control.Applicative.empty
  "!"  -> trace "Error !" Control.Applicative.empty
  _    -> trace "Error with UnaryOp" Control.Applicative.empty
generateExpression (Identifier name) = getPreLoadedOperandForIdentifier name
generateExpression (AST.Call expr exprs) = callOperation expr exprs toCall
generateExpression (Primary expr) = trace "Error prim" Control.Applicative.empty

generateAddrIdentifier :: KExpr -> Codegen (Maybe Operand)
generateAddrIdentifier (Identifier name) = do
  op <- getAddrOperandForIdentifier name
  return $ Just op
generateAddrIdentifier _ = return Nothing

getNameFromIdentifier :: KExpr -> Maybe String
getNameFromIdentifier (Identifier name) = Just name
getNameFromIdentifier _ = Nothing

toNumOperand :: Double -> Codegen Operand
toNumOperand value = return $ ConstantOperand $ C.Float $ F.Double value

mathOperation :: KExpr -> KExpr -> (Operand -> Operand -> Instruction) -> Codegen Operand
mathOperation lhs rhs op = do
  first  <- generateExpression lhs
  second <- generateExpression rhs
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
  first <- generateExpression lhs
  second <- generateExpression rhs
  instr double (FCmp op first second [])


toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = Prelude.map (, [])


toCall :: Operand -> [Operand] -> Instruction
toCall fn args = I.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

iterateN :: Int -> a -> [a]
iterateN 0 _ = []
iterateN n value = [value] ++ iterateN (n - 1) value

getFunctionType :: Type -> Int -> Type
getFunctionType ret nb = PointerType { pointerReferent = FunctionType { resultType = ret, argumentTypes = iterateN nb ret, isVarArg = Prelude.False }, pointerAddrSpace = AddrSpace 0}

linkToFunction :: AST.Name -> Int -> Operand
linkToFunction name nb = extern (getFunctionType double nb) $ mkName name

callOperation :: KExpr -> [KExpr] -> (Operand -> [Operand] -> Instruction) -> Codegen Operand
callOperation fn args op = do
  let fnName = getNameFromIdentifier fn
  argsCodegen <- forM args \a -> do
    generateExpression a
  case fnName of
    Just name -> instr double $ toCall (linkToFunction name $ length argsCodegen) argsCodegen
    Nothing -> error "Invalid function name"


findOperandInSymtab :: String -> [(String, Operand)] -> Maybe Operand
findOperandInSymtab _ [] = Nothing
findOperandInSymtab value ((name, op):xs)
  | name == value = Just op
  | otherwise = findOperandInSymtab value xs


addToSymtab :: AST.Name -> Codegen Operand
addToSymtab name = do
  newName <- freshSuggestedName name
  addr <- toAlloca
  symTab <- CodegenT $ gets symTab
  CodegenT $ modify $ \s -> ( s { symTab = symTab ++ [(name, addr)]} )
  return addr

getPreLoadedOperandForIdentifier :: AST.Name -> Codegen Operand
getPreLoadedOperandForIdentifier name = do
  symbols <- CodegenT $ gets symTab
  case findOperandInSymtab name symbols of
    Just addr -> toLoad addr
    Nothing -> addToSymtab name


getAddrOperandForIdentifier :: AST.Name -> Codegen Operand
getAddrOperandForIdentifier name = do
  symbols <- CodegenT $ gets symTab
  case findOperandInSymtab name symbols of
    Just addr -> return addr
    Nothing -> addToSymtab name


toAlloca :: Codegen Operand
toAlloca = instr double $ I.Alloca double Nothing 0 []


toLoad :: Operand -> Codegen Operand
toLoad ptr = instr double $ I.Load Prelude.False ptr Nothing 0 []


toStore :: Operand -> Operand -> Codegen Operand
toStore ptr value = do
  instr VoidType $ I.Store Prelude.False ptr value Nothing 0 []
  return zero


toRet :: Operand -> Codegen (Named Terminator)
toRet value = terminator $ Do $ Ret (Just value) []


toBr :: AST.Name -> Codegen (Named Terminator)
toBr name = terminator $ Do $ Br (mkName name) []


toCondBr :: Operand -> AST.Name -> AST.Name -> Codegen (Named Terminator)
toCondBr cond trueId falseId = terminator $ Do $ CondBr cond (mkName trueId) (mkName falseId) []


toPhi :: Type -> [(Operand, LLVM.AST.Name)] -> Codegen Operand
toPhi type' values = instr type' $ I.Phi type' values []


extern :: Type -> LLVM.AST.Name -> Operand
extern type' name = ConstantOperand $ C.GlobalReference type' name


assignOperation :: KExpr -> KExpr -> Codegen Operand
assignOperation lhs rhs = do
  first <- generateAddrIdentifier lhs
  second <- generateExpression rhs
  case first of
    Just value -> toStore value second
    Nothing -> error "It's not an identifier"
  return zero

getStringFromName :: LLVM.AST.Name -> Maybe String
getStringFromName (Name value) = Just $ show value
getStringFromName (UnName value) = Just $ show value

getOperandName :: Operand -> Maybe LLVM.AST.Name
getOperandName (LocalReference _ name) = Just name
getOperandName (ConstantOperand _) = Nothing
getOperandName (MetadataOperand _) = Nothing
