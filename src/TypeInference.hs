module TypeInference
    ( infereType
    )
where

import           Control.Exception
import           Debug.Trace
import           State
import           AST
import qualified Data.Map                      as Map

type Error = String

type Context = (Map.Map String (Maybe (Either String KType)))

infereType :: [KDefs] -> Either Error [KDefs]
infereType e = trace "Start " (unify e . collectConstraints e . annotate $ e)

--- Annotation

annotate :: [KDefs] -> Context
annotate = foldr annotateKdef Map.empty

annotateKdef :: KDefs -> Context -> Context
annotateKdef kdef ctx = case kdef of
    Def name vars ty expr ->
        Map.insert name (Right <$> ty) (annotateVariables vars ctx)
    Expressions kexprs -> ctx

annotateVariables :: [VariableDef] -> Context -> Context
annotateVariables vars ctx = foldr annotateVariableDef ctx vars

annotateVariableDef :: VariableDef -> Context -> Context
annotateVariableDef (VariableDef name ty) = Map.insert name (Right <$> ty)

annotateKExprs :: KExprs -> Context -> Context
annotateKExprs kexprs ctx = case kexprs of
    For var1 val1 cond1 cond2 step exprs ->
        annotateKExprs exprs
            $ annotateKExpr step
            $ annotateKExpr cond1
            $ annotateKExpr var1 ctx
    If cond action elseAction -> case elseAction of
        Just elseA ->
            annotateKExprs elseA $ annotateKExprs action $ annotateKExpr
                cond
                ctx
        Nothing -> annotateKExprs action $ annotateKExpr cond ctx
    While cond action   -> annotateKExprs action $ annotateKExpr cond ctx
    Expression listExpr -> foldr annotateKExpr ctx listExpr

annotateManyKExpr :: [KExpr] -> Context -> Context
annotateManyKExpr exprs ctx = foldr annotateKExpr ctx exprs

annotateKExpr :: KExpr -> Context -> Context
annotateKExpr kexpr ctx = case kexpr of
    Int   x                  -> ctx
    Float y                  -> ctx
    BinaryOp name expr expr1 -> annotateBinary name expr expr1 ctx
    UnaryOp name expr        -> annotateKExpr expr ctx
    Identifier name          -> ctx
    Call expr listExpr -> annotateKExpr expr $ annotateManyKExpr listExpr ctx
    Primary exprs            -> annotateKExprs exprs ctx

annotateBinary :: String -> KExpr -> KExpr -> Context -> Context
annotateBinary name ex1 ex2 ctx = case (ex1, ex2) of
    (Identifier x, Identifier y) -> Map.insert x (Just $ Left y) ctx
    (Identifier x, Call name _) ->
        Map.insert x (Just . Left $ getIdentifier name) ctx
    (Identifier x, expr) -> Map.insert x Nothing ctx
    _                    -> ctx

getIdentifier :: KExpr -> String
getIdentifier expr = case expr of
    Identifier name -> name
    _               -> error "Error function call is not an identifier."

--- Collect constraints

collectConstraints :: [KDefs] -> Context -> Context
collectConstraints expr ctx =  trace ("collectConstraints  ")  (parcourKdef expr 0 ctx)


checkExistingElemCtx :: String -> Context -> Bool
checkExistingElemCtx name ctx
  | show (Map.lookup name ctx) == "Nothing" = False
  | otherwise = True


parcourKdef ::  [KDefs] -> Int -> Context -> Context
parcourKdef kdefs offset ctx
  | (length kdefs) <= offset = ctx
  | otherwise = trace (show (kdefs !! offset)) (parcourKdef kdefs (offset + 1) (collectConstraintsKDefs (kdefs !! offset) ctx))


collectConstraintsKDefs :: KDefs -> Context -> Context
collectConstraintsKDefs kdef ctx = case kdef of
    Def name vars ty expr -> trace ("Def: " ++ show kdef) (ctx)
    Expressions kexprs     -> trace ("Expr: " ++ show kdef) (ctx)




---(Map.lookup var ctx)
--- Unify

unify :: [KDefs] -> Context -> Either Error [KDefs]
unify expr ctx = traceMap ctx $ Right expr

traceMap :: Context -> a -> a
traceMap ctx = trace $ unwords $ map show (Map.toAscList ctx)
