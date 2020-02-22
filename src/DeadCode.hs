module DeadCode
    ( removeDeadCode
    )
where

import           Data.Maybe
import           Debug.Trace
import           AST
import           Data.Map                      as Map

type Context = [String]

-- Remove dead code from previously parsed expression
removeDeadCode :: [KDefs] -> [KDefs]
removeDeadCode kdefs = removeUnused kdefs . annotate kdefs $ []

-- Note used definitions
annotate :: [KDefs] -> Context -> Context
annotate = annotateManyKDefs

annotateManyKDefs :: [KDefs] -> Context -> Context
annotateManyKDefs kdefs ctx = Prelude.foldr annotateKdef ctx kdefs

annotateKdef :: KDefs -> Context -> Context
annotateKdef kdef ctx = case kdef of
    Def name vars _ exprs ->
        annotateKExprs exprs $ annotateManyVariables vars ctx
    Expressions kexprs -> annotateKExprs kexprs ctx

annotateManyVariables :: [VariableDef] -> Context -> Context
annotateManyVariables vars ctx = Prelude.foldr annotateVariables ctx vars

annotateVariables :: VariableDef -> Context -> Context
annotateVariables (VariableDef name _) = (name :)

annotateManyKExpr :: [KExpr] -> Context -> Context
annotateManyKExpr kexprs ctx = Prelude.foldr annotateKExpr ctx kexprs

annotateKExpr :: KExpr -> Context -> Context
annotateKExpr kexpr ctx = case kexpr of
    Identifier name               -> name : ctx
    Call (Identifier name) kexprs -> name : annotateManyKExpr kexprs ctx
    _                             -> ctx

annotateKExprs :: KExprs -> Context -> Context
annotateKExprs kexprs ctx = case kexprs of
    For e1 e2 e3 e4 e5 e6 ->
        annotateKExprs e6
            $ annotateKExpr e5
            $ annotateKExpr e4
            $ annotateKExpr e3
            $ annotateKExpr e2
            $ annotateKExpr e1 ctx
    If e1 e2 e3 -> case e3 of
        Just e  -> annotateKExprs e $ annotateKExprs e2 $ annotateKExpr e1 ctx
        Nothing -> annotateKExprs e2 $ annotateKExpr e1 ctx
    While e1 e2  -> annotateKExprs e2 $ annotateKExpr e1 ctx
    Expression e -> annotateManyKExpr e ctx

-- Remove Unused
removeUnused :: [KDefs] -> Context -> [KDefs]
removeUnused kdefs ctx = Data.Maybe.mapMaybe (checkRemove ctx) kdefs

checkRemove :: Context -> KDefs -> Maybe KDefs
checkRemove names def@(Def name _ _ _) =
    if name `elem` names then Just def else Nothing
checkRemove names expr@(Expressions exprs) = Just expr

