module RewriteSim.Example.ABC (example) where

import Data.Tuple (Tuple(..))
import RewriteSim.Example.Common (Example)

example :: Example
example = Tuple "ABC" { rules: [], exprs: [] }

-- example
--   :: Tuple String
--        { exprs :: Array (String /\ Expr)
--        , rules :: Array (Expr -> Maybe Expr)
--        }
-- example = Tuple "ACB"
--   { rules:
--       [ case _ of
--           "A" % [] -> Just $ "B" % []
--           _ -> Nothing
--       , case _ of
--           "B" % [] -> Just $ "C" % []
--           _ -> Nothing
--       , case _ of
--           "C" % [] -> Just $ "A" % []
--           _ -> Nothing
--       ]
--   , exprs:
--       [ Tuple "ex1" $
--           "A" % []
--       ]
--   }

