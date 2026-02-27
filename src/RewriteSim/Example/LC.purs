module RewriteSim.Example.LC where

import Prelude

import Data.Tuple (Tuple(..))
import RewriteSim (me, newRule, (%))
import RewriteSim.Example.Common (Example, GenericExpr)

app :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
app f a = "app" % [ f, a ]

lam :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
lam x b = "lam" % [ x, b ]

var :: forall x. GenericExpr x -> GenericExpr x
var x = "var" % [ x ]

lit :: forall x. String -> GenericExpr x
lit s = s % []

subst :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x -> GenericExpr x
subst x v a = "subst" % [ x, v, a ]

-- TODO: the way to avoid name collisions and difference checks is just to use DeBruijn indices
example :: Example
example = Tuple "LC"
  { rules:
      [ newRule "beta"
          (app (lam (me "x") (me "b")) (me "a"))
          (subst (me "x") (me "a") (me "b"))
      --   
      -- subst/app
      --   
      , newRule "subst/app"
          (subst (me "x") (me "v") (app (me "f") (me "a")))
          (app (subst (me "x") (me "v") (me "f")) (subst (me "x") (me "v") (me "a")))
      --   
      -- subst/lam
      --   
      -- TODO: deal with colliding names
      , newRule "subst/lam"
          (subst (me "x") (me "v") (lam (me "x") (me "b")))
          (lam (me "x") (subst (me "x") (me "v") (me "b")))
      --   
      -- subst/var
      --   
      -- TODO: deal with case where x /= y
      , newRule "subst/var"
          (subst (me "x") (me "v") (var (me "x")))
          (me "v")
      ]
  , exprs:
      [ Tuple "ex1" $
          "app" % [ "lam" % [ "x" % [], "var" % [ "x" % [] ] ], "var" % [ "a" % [] ] ]
      ]
  }
