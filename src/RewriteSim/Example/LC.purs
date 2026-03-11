module RewriteSim.Example.LC where

import Prelude

import RewriteSim (me, newRule, (%))
import RewriteSim.Example.Common (Example, GenericExpr)

app :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
app f a = "app" % [ f, a ]

lam :: forall x. GenericExpr x -> GenericExpr x
lam b = "lam" % [ b ]

var :: forall x. GenericExpr x -> GenericExpr x
var x = "var" % [ x ]

suc :: forall x. GenericExpr x -> GenericExpr x
suc x = "suc" % [ x ]

zero :: forall x. GenericExpr x
zero = "zero" % []

-- lit :: forall x. String -> GenericExpr x
-- lit s = s % []

subst :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x -> GenericExpr x
subst x v a = "subst" % [ x, v, a ]

example :: Example
example =
  { name: "LC"
  , rules:
      [ newRule "beta"
          (app (lam (me "b")) (me "a"))
          (subst zero (me "a") (me "b"))
      , newRule "subst/app"
          (subst (me "x") (me "v") (app (me "f") (me "a")))
          (app (subst (me "x") (me "v") (me "f")) (subst (me "x") (me "v") (me "a")))
      , newRule "subst/lam"
          (subst (me "x") (me "v") (lam (me "b")))
          (lam (subst (suc (me "x")) (me "v") (me "b")))
      , newRule "subst/var/zero"
          (subst zero (me "v") (var (me "x")))
          (me "v")
      , newRule "subst/var/suc"
          (subst (suc (me "y")) (me "v") (var (me "x")))
          (var (me "x"))
      ]
  , tests:
      [ { name: "lam-app"
        , input: app (lam (var zero)) (var zero)
        , output: lam (var zero)
        }
      , { name: "lam-app-2"
        , input: app (app (lam (lam (var zero))) (var zero)) (var (suc zero))
        , output: lam (var zero)
        }
      ]
  }
