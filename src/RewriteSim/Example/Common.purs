module RewriteSim.Example.Common where

import RewriteSim as RS

--------------------------------------------------------------------------------

type Expr = RS.Expr String
type AbsExpr = RS.AbsExpr String
type GenericExpr x = RS.GenericExpr x String
type Rule = RS.Rule String

type System = RS.System String
type GlobalUpdate = RS.GlobalUpdate String

type Example =
  { name :: String
  , rules :: Array Rule
  , tests :: Array ExampleTest
  }

type ExampleTest =
  { name :: String
  , input :: Expr
  , output :: Expr
  }

me :: String -> GenericExpr RS.MetaVar
me = RS.me
