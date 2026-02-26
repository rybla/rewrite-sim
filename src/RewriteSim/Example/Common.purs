module RewriteSim.Example.Common where

import Data.Tuple.Nested (type (/\))
import RewriteSim as RS

type Expr = RS.Expr String
type AbsExpr = RS.AbsExpr String
type GenericExpr x = RS.GenericExpr x String
type Rule = RS.Rule String
type Example =
  String /\
    { rules :: Array Rule
    , exprs :: Array (String /\ Expr)
    }

type System = RS.System String
type GlobalUpdate = RS.GlobalUpdate String
