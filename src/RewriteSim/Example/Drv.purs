module RewriteSim.Example.Drv where

import RewriteSim.Example.Common (Expr)

type SortSystem s =
  { kidSortsOfExpr :: Expr -> Array s
  , sortOfExpr :: Expr -> s
  }

-- type Rule = {

-- }


checkDerivation :: Expr -> 

-- checkExpr ::  Expr -> m Expr


