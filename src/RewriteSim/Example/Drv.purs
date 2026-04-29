module RewriteSim.Example.Drv where

import RewriteSim.Example.Common (Expr)

type SortSystem s =
  { kidSortsOfExpr :: Expr -> Array s
  , sortOfExpr :: Expr -> s
  }

-- kidSortsOfExpr :: 

-- class IsExpr e s | e  -> s where
--   kidSortsOfExpr  :: e  -> Array s
--   sortOfExpr  :: e  -> s
--   showExpr :: GenericExpr 

-- checkExpr :: 

