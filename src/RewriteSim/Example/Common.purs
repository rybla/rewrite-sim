module RewriteSim.Example.Common where

import Prelude
import RewriteSim as RS

--------------------------------------------------------------------------------

-- newtype ExprLabel = ExprLabel String

-- derive instance Eq ExprLabel

-- instance Show ExprLabel where
--   show (ExprLabel l) = l

type ExprLabel = String

type Expr = RS.Expr ExprLabel
type AbsExpr = RS.AbsExpr ExprLabel
type GenericExpr x = RS.GenericExpr x ExprLabel
type Rule = RS.Rule ExprLabel

type System = RS.System ExprLabel
type GlobalUpdate = RS.GlobalUpdate ExprLabel

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
