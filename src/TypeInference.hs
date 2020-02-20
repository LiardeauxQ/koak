module TypeInference
    ( infereType
    )
where

import           Debug.Trace
import           State
import           AST
import qualified Data.Map                      as Map

type Error = String

type Context = (Map.Map String (Maybe KType))

infereType :: [KDefs] -> Either Error [KDefs]
infereType e = unify e . collectConstraints e . annotate $ e

annotate :: [KDefs] -> Context
annotate = foldr annotateKdef Map.empty

annotateKdef :: KDefs -> Context -> Context
annotateKdef kdef ctx = case kdef of
    Def name vars ty expr -> Map.insert name ty (annotateVariables vars ctx) -- TODO change
    _                     -> ctx

annotateVariables :: [VariableDef] -> Context -> Context
annotateVariables vars ctx = foldr annotateVariableDef ctx vars

annotateVariableDef :: VariableDef -> Context -> Context
annotateVariableDef (VariableDef name ty) = Map.insert name ty

annotateKExprs :: KExprs -> Context -> Context
annotateKExprs kexprs ctx = case kexprs of
    For var1 val1 cond1 cond2 step exprs -> ctx
    If cond action elseAction            -> ctx
    While cond action                    -> ctx
    Expression listExpr                  -> ctx


annotateKExpr :: Context -> KExpr -> Context
annotateKExpr ctx kexpr = case kexpr of
    Int   x                  -> ctx
    Float y                  -> ctx
    BinaryOp name expr expr1 -> annotateBinary name expr expr1 ctx
    UnaryOp name expr        -> ctx
    Identifier name          -> ctx
    Call expr listExpr       -> ctx
    Primary exprs            -> annotateKExprs exprs ctx

annotateBinary :: String -> KExpr -> KExpr -> Context -> Context
annotateBinary name ex1 ex2 ctx = case ex1 of
    Identifier x -> Map.insert x Nothing ctx
    _            -> ctx

collectConstraints :: [KDefs] -> Context -> Context
collectConstraints expr ctx = trace (concatMap show ctx) ctx

unify :: [KDefs] -> Context -> Either Error [KDefs]
unify expr ctx = Right []
