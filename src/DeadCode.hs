module DeadCode
    ( removeDeadCode
    )
where

import           AST

-- Remove dead code from final parsed expression
removeDeadCode :: [KDefs] -> [KDefs]
removeDeadCode kdefs = kdefs
