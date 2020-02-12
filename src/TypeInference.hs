module TypeInference
    ()
where

import           State
import           AST
import           Data.Map

type Error = String

-- Type context.
type TypeState = StateT (Map String KType) (Either Error) [KDefs]

infereType :: [KDefs] -> [KDefs]
infereType = annotate . collectConstraints . unify

annotate :: [KDefs] -> [KDefs]
annotate = id

collectConstraints :: [KDefs] -> [KDefs]
collectConstraints = id

unify :: [KDefs] -> [KDefs]
unify = id
