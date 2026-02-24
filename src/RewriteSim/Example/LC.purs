module RewriteSim.Example.LC where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import RewriteSim.Example.Common (Expr)
import RewriteSim ((%))

example
  :: Tuple String
       { exprs :: Array (String /\ Expr)
       , rules :: Array (Expr -> Maybe Expr)
       }
example = Tuple "LC"
  { rules:
      [ case _ of
          "app" % [ "lam" % [ x % [], b ], a ] -> pure $ subst x a b
          _ -> Nothing
      ]
  , exprs:
      [ Tuple "ex1" $
          "app" % [ "lam" % [ "x" % [], "var" % [ "x" % [] ] ], "var" % [ "a" % [] ] ]
      ]
  }

subst :: String -> Expr -> Expr -> Expr
subst x a ("var" % [ y % [] ]) | x == y = a
subst x a (l % es) = l % (es # map (subst x a))
