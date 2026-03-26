module RewriteSim.Example.ISLSCv0d2d2 where

import Prelude hiding (zero)

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Variant (Variant)
import Data.Variant as Variant
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim ((%))
import RewriteSim as RS
import RewriteSim.Example.Common (Example, GenericExpr, Rule, me)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Terms, Substitutions, Contexts
--------------------------------------------------------------------------------

-- terms

zeroTerm :: forall x. GenericExpr x
zeroTerm = "zeroTerm" % []

lamTerm :: forall x. GenericExpr x -> GenericExpr x
lamTerm b = "lam" % [ b ]

appTerm :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
appTerm f a = "app" % [ f, a ]

subTerm :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
subTerm s a = "sub" % [ s, a ]

holeTerm :: forall x. GenericExpr x
holeTerm = "holeTerm" % []

-- contexts

zeroCtx :: forall x. GenericExpr x
zeroCtx = "zeroCtx" % []

sucCtx :: forall x. GenericExpr x -> GenericExpr x
sucCtx n = "sucCtx" % [ n ]

holeCtx :: forall x. GenericExpr x
holeCtx = "holeCtx" % []

-- substitutions

shiftSub :: forall x. GenericExpr x
shiftSub = "shift" % []

extendSub :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
extendSub s a = "extend" % [ s, a ]

composeSub :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
composeSub s t = "compose" % [ s, t ]

holeSub :: forall x. GenericExpr x
holeSub = "holeSub" % []

--------------------------------------------------------------------------------
-- meta-functions
--------------------------------------------------------------------------------

type ErrorRow x =
  ( invalid :: { sort :: String, expr :: GenericExpr x }
  , meta :: { sort :: String, var :: x }
  )

getIndicesOfDer
  :: forall m x
   . MonadError (Variant (ErrorRow x)) m
  => GenericExpr x
  -> m { n :: GenericExpr x, a :: GenericExpr x }
getIndicesOfDer (RS.MetaExpr var) = throwError $ Variant.inj (Proxy @"meta") { sort: "Der", var }
getIndicesOfDer ("Zero" % [ n ]) = pure { n: sucCtx n, a: zeroTerm }
getIndicesOfDer ("Lam" % [ n, b, _bDer ]) = pure { n: n, a: lamTerm b }
getIndicesOfDer ("App" % [ n, f, a, _fDer, _aDer ]) = pure { n: n, a: appTerm f a }
getIndicesOfDer ("Sub" % [ _m, n, s, a, _sDer, _aDer ]) = pure { n: n, a: subTerm s a }
getIndicesOfDer ("Boundary" % [ n, _a, b, _aDer ]) = pure { n: n, a: b }
getIndicesOfDer ("HoleTerm" % [ n, a ]) = pure { n: n, a: a }
getIndicesOfDer expr = throwError $ Variant.inj (Proxy @"invalid") { sort: "Der", expr }

--------------------------------------------------------------------------------
-- Example Derivations
--------------------------------------------------------------------------------

shiftDerFn
  :: forall m x
   . MonadError (Variant (ErrorRow x)) m
  => Show x
  => GenericExpr x
  -> m (GenericExpr x)
shiftDerFn der = do
  { n, a } <- getIndicesOfDer der
  pure $
    subDer { m: n, n: sucCtx n, s: shiftSub, a: a }
      (shiftDer { n: n })
      der

deBruijn :: forall m @x. MonadError (Variant (ErrorRow x)) m => Show x => Int -> { n :: GenericExpr x } -> m (GenericExpr x)
deBruijn i { n } =
  if i < 0 then unsafeCrashWith $ "deBruijn with negative index: " <> show i
  else if i == 0 then
    pure $ zeroDer { n }
  else
    shiftDerFn =<< deBruijn (i - 1) { n }

oneDer
  :: forall m @x
   . MonadError (Variant (ErrorRow x)) m
  => Show x
  => { n :: GenericExpr x }
  -> m (GenericExpr x)
oneDer { n } = shiftDerFn (zeroDer { n: n })

twoDer
  :: forall m @x
   . MonadError (Variant (ErrorRow x)) m
  => Show x
  => { n :: GenericExpr x }
  -> m (GenericExpr x)
twoDer { n } = shiftDerFn =<< oneDer { n: n }

oneDer' :: forall x. { n :: GenericExpr x } -> GenericExpr x
oneDer' { n } =
  subDer { m: sucCtx n, n: sucCtx (sucCtx n), s: shiftSub, a: zeroTerm }
    (shiftDer { n: sucCtx n })
    (zeroDer { n: n })

twoDer' :: forall x. { n :: GenericExpr x } -> GenericExpr x
twoDer' { n } =
  subDer { m: sucCtx (sucCtx n), n: sucCtx (sucCtx (sucCtx n)), s: shiftSub, a: subTerm shiftSub zeroTerm }
    (shiftDer { n: sucCtx (sucCtx n) })
    (oneDer' { n: n })

--------------------------------------------------------------------------------
-- Derivation Rules
--------------------------------------------------------------------------------

-- derivation rules for top

topDer :: forall x. { n :: GenericExpr x, a :: GenericExpr x } -> GenericExpr x -> GenericExpr x
topDer { n, a } aDer = "Top" % [ n, a, aDer ]

-- derivation rules for terms

zeroDer :: forall x. { n :: GenericExpr x } -> GenericExpr x
zeroDer { n } = "Zero" % [ n ]

lamDer :: forall x. { n :: GenericExpr x, b :: GenericExpr x } -> GenericExpr x -> GenericExpr x
lamDer { n, b } bDer = "Lam" % [ n, b, bDer ]

appDer :: forall x. { n :: GenericExpr x, f :: GenericExpr x, a :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
appDer { n, f, a } fDer aDer = "App" % [ n, f, a, fDer, aDer ]

subDer :: forall x. { a :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
subDer { m, n, s, a } sDer aDer = "Sub" % [ m, n, s, a, sDer, aDer ]

boundaryTermDer :: forall x. { a :: GenericExpr x, b :: GenericExpr x, n :: GenericExpr x } -> GenericExpr x -> GenericExpr x
boundaryTermDer { n, a, b } aDer = "BoundaryTerm" % [ n, a, b, aDer ]

boundarySubDer :: forall x. { m :: GenericExpr x, n :: GenericExpr x, t :: GenericExpr x, s :: GenericExpr x } -> GenericExpr x -> GenericExpr x
boundarySubDer { m, n, t, s } sDer = "BoundaryTerm" % [ m, n, t, s, sDer ]

holeTermDer :: forall x. { n :: GenericExpr x, a :: GenericExpr x } -> GenericExpr x
holeTermDer { n, a } = "HoleTerm" % [ n, a ]

-- derivation rules for substitutions

shiftDer :: forall x. { n :: GenericExpr x } -> GenericExpr x
shiftDer { n } = "Shift" % [ n ]

extendDer :: forall x. { a :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
extendDer { m, n, s, a } sDer aDer = "Extend" % [ m, n, s, a, sDer, aDer ]

composeDer :: forall x. { l :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x, t :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
composeDer { l, m, n, t, s } sDer tDer = "Compose" % [ l, m, n, t, s, sDer, tDer ]

holeSubDer :: forall x. { m :: GenericExpr x, n :: GenericExpr x, a :: GenericExpr x } -> GenericExpr x
holeSubDer { m, n, a } = "HoleSub" % [ m, n, a ]

--------------------------------------------------------------------------------
-- Propagation Rules
--------------------------------------------------------------------------------

rules :: Array Rule
rules =
  [ ----------------------------------------------------------------------------
    -- Substitution Propagation Rules
    ----------------------------------------------------------------------------
    let
      _n = me "n"
      _m = me "m"
      _s = me "s"
      _b = me "b"
      _S = me "S"
      _B = me "B"
    in
      RS.Rule
        { name: "[s] (λ b) ~~> λ [(↑ ∘ s) ⋅ zero] b"
        , input:
            subDer { m: _m, n: _n, s: _s, a: lamTerm _b }
              _S
              ( lamDer { n: _m, b: _b }
                  _B
              )
        , output:
            boundaryTermDer { n: _n, a: lamTerm (subTerm ((shiftSub `composeSub` _s) `extendSub` zeroTerm) _b), b: subTerm _s (lamTerm _b) }
              ( lamDer { n: _n, b: subTerm ((shiftSub `composeSub` _s) `extendSub` zeroTerm) _b }
                  ( subDer { m: sucCtx _m, n: sucCtx _n, s: (shiftSub `composeSub` _s) `extendSub` zeroTerm, a: _b }
                      ( extendDer { m: _m, n: sucCtx _n, s: shiftSub `composeSub` _s, a: zeroTerm }
                          ( composeDer { l: _n, m: _n, n: sucCtx _n, s: shiftSub, t: _s }
                              (shiftDer { n: _n })
                              _S
                          )
                          (zeroDer { n: _n })
                      )
                      _B
                  )
              )
        }
  , let
      _n = me "n"
      _m = me "m"
      _s = me "s"
      _f = me "f"
      _a = me "a"
      _S = me "S"
      _F = me "F"
      _A = me "A"
    in
      RS.Rule
        { name: "[s] (f a) ~~> [s] f [s] a"
        , input:
            subDer { m: _m, n: _n, s: _s, a: appTerm _f _a }
              _S
              ( appDer { n: _m, f: _f, a: _a }
                  _F
                  _A
              )
        , output:
            boundaryTermDer { n: _n, a: appTerm (subTerm _s _f) (subTerm _s _a), b: subTerm _s (appTerm _f _a) }
              ( appDer { n: _n, f: subTerm _s _f, a: subTerm _s _a }
                  ( subDer { m: _m, n: _n, s: _s, a: _f }
                      _S
                      _F
                  )
                  ( subDer { m: _m, n: _n, s: _s, a: _a }
                      _S
                      _A
                  )
              )
        }
  , let
      _m = me "m"
      _n = me "n"
      _s = me "s"
      _a = me "a"
      _S = me "S"
      _A = me "A"
    in
      RS.Rule
        { name: "[s ⋅ a] zero ~~> a"
        , input:
            subDer { m: sucCtx _m, n: _n, s: _s, a: _a }
              ( extendDer { m: _m, n: _n, s: _s, a: _a }
                  _S
                  _A
              )
              (zeroDer { n: _m })
        , output:
            boundaryTermDer { n: _n, a: _a, b: subTerm (extendSub _s _a) zeroTerm }
              _A

        }
  , let
      _l = me "l"
      _m = me "m"
      _n = me "n"
      _t = me "t"
      _s = me "s"
      _a = me "a"
      _T = me "T"
      _S = me "S"
      _A = me "A"
    in
      RS.Rule
        { name: "[t] [s] a ~~> [t ∘ s] a"
        , input:
            subDer { m: _m, n: _n, s: _t, a: subTerm _s _a }
              _T
              ( subDer { m: _l, n: _m, s: _s, a: _a }
                  _S
                  _A
              )
        , output:
            boundaryTermDer { n: _n, a: subTerm (composeSub _t _s) _a, b: subTerm _t (subTerm _s _a) }
              ( subDer { m: _l, n: _n, s: composeSub _t _s, a: _a }
                  ( composeDer { l: _l, m: _m, n: _n, t: _t, s: _s }
                      _T
                      _S
                  )
                  _A
              )
        }
  , let
      _k = me "k"
      _l = me "l"
      _m = me "m"
      _n = me "n"
      _u = me "u"
      _t = me "t"
      _s = me "s"
      _U = me "U"
      _T = me "T"
      _S = me "S"
    in
      RS.Rule
        { name: "u ∘ (t ∘ s) ~~> (u ∘ t) ∘ s"
        , input:
            composeDer { l: _k, m: _m, n: _n, t: _u, s: composeSub _t _s }
              _U
              ( composeDer { l: _k, m: _l, n: _m, t: _t, s: _s }
                  _T
                  _S
              )
        , output:
            boundarySubDer { m: _k, n: _n, t: composeSub (composeSub _u _t) _s, s: composeSub _u (composeSub _t _s) }
              ( composeDer { l: _k, m: _l, n: _n, t: composeSub _u _t, s: _s }
                  ( composeDer { l: _m, m: _m, n: _n, t: _u, s: _t }
                      _U
                      _T
                  )
                  _S
              )
        }
  , let
      _l = me "l"
      _m = me "m"
      _n = me "n"
      _t = me "t"
      _s = me "s"
      _a = me "a"
      _T = me "T"
      _S = me "S"
      _A = me "A"
    in
      RS.Rule
        { name: "t ∘ s ⋅ a ~~> (t ∘ s) ⋅ [t] a"
        , input:
            composeDer { l: _m, m: sucCtx _m, n: _n, t: extendSub _s _a, s: shiftSub }
              _T
              ( extendDer { m: _m, n: _n, s: _s, a: _a }
                  _S
                  _A
              )
        , output:
            boundarySubDer { m: sucCtx _l, n: _n, t: extendSub (composeSub _t _s) (subTerm _t _a), s: composeSub _t (extendSub _s _a) }
              ( extendDer { m: _l, n: _n, s: composeSub _t _s, a: subTerm _t _a }
                  ( composeDer { l: _l, m: _m, n: _n, t: _t, s: _s }
                      _T
                      _S
                  )
                  ( subDer { m: _m, n: _n, s: _t, a: _a }
                      _T
                      _A
                  )
              )
        }
  , let
      _m = me "m"
      _n = me "n"
      _s = me "s"
      _a = me "a"
      _S = me "S"
      _A = me "A"
    in
      RS.Rule
        { name: "(s ⋅ a) ∘ ↑ ~~> s"
        , input:
            composeDer { l: _m, m: sucCtx _m, n: _n, t: shiftSub, s: extendSub _s _a }
              ( extendDer { m: _m, n: _n, s: _s, a: _a }
                  _S
                  _A
              )
              (shiftDer { n: _m })
        , output:
            boundarySubDer { m: _m, n: _n, t: _s, s: composeSub (extendSub _s _a) shiftSub }
              _S
        }
  ----------------------------------------------------------------------------
  -- Boundary Propagation Rules
  ----------------------------------------------------------------------------
  , let
      _n = me "n"
      _b = me "b"
      _b' = me "b'"
      _B = me "B"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over Lam at b"
        , input:
            lamDer { n: _n, b: _b' }
              ( boundaryTermDer { n: _n, a: _b, b: _b' }
                  _B
              )
        , output:
            boundaryTermDer { n: _n, a: _b, b: _b' }
              ( lamDer { n: _n, b: _b' }
                  _B
              )
        }
  , let
      _n = me "n"
      _f = me "f"
      _f' = me "f'"
      _a = me "a"
      _F = me "F"
      _A = me "A"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over App at f"
        , input:
            appDer { n: _n, f: _f', a: _a }
              ( boundaryTermDer { n: _n, a: _f, b: _f' }
                  _F
              )
              _A
        , output:
            boundaryTermDer { n: _n, a: appTerm _f _a, b: appTerm _f' _a }
              ( appDer { n: _n, f: _f, a: _a }
                  _F
                  _A
              )
        }
  , let
      _n = me "n"
      _f = me "f"
      _a = me "a"
      _a' = me "a'"
      _F = me "F"
      _A = me "A"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over App at a"
        , input:
            appDer { n: _n, f: _f, a: _a' }
              _F
              ( boundaryTermDer { n: _n, a: _a, b: _a' }
                  _A
              )
        , output:
            boundaryTermDer { n: _n, a: appTerm _f _a, b: appTerm _f _a' }
              ( appDer { n: _n, f: _f, a: _a }
                  _F
                  _A
              )
        }
  , let
      _m = me "m"
      _n = me "n"
      _s = me "s"
      _s' = me "s'"
      _a = me "a"
      _S = me "S"
      _A = me "A"
    in
      RS.Rule
        { name: "propagate BoundarySub over Sub at s"
        , input:
            subDer { m: _m, n: _n, s: _s', a: _a }
              ( boundarySubDer { m: _m, n: _n, t: _s, s: _s' }
                  _S
              )
              _A
        , output:
            boundaryTermDer { n: _n, a: subTerm _s _a, b: subTerm _s' _a }
              ( subDer { m: _m, n: _n, s: _s, a: _a }
                  _S
                  _A
              )
        }
  , let
      _m = me "m"
      _n = me "n"
      _s = me "s"
      _a = me "a"
      _a' = me "a'"
      _S = me "S"
      _A = me "A"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over Sub at a"
        , input:
            subDer { m: _m, n: _n, s: _s, a: _a' }
              _S
              ( boundaryTermDer { n: _m, a: _a, b: _a' }
                  _A
              )
        , output:
            boundaryTermDer { n: _n, a: subTerm _s _a', b: subTerm _s _a' }
              ( subDer { m: _m, n: _n, s: _s, a: _a }
                  _S
                  _A
              )
        }
  , let
      _m = me "m"
      _n = me "n"
      _s = me "s"
      _s' = me "s'"
      _a = me "a"
      _S = me "S"
      _A = me "A"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over Extend at s"
        , input:
            extendDer { m: _m, n: _n, s: _s', a: _a }
              ( boundarySubDer { m: _m, n: _n, t: _s, s: _s' }
                  _S
              )
              _A
        , output:
            boundarySubDer { m: _m, n: _n, t: extendSub _s _a, s: extendSub _s' _a }
              ( extendDer { m: _m, n: _n, s: _s, a: _a }
                  _S
                  _A
              )
        }
  , let
      _n = me "n"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over Extend at a"
        , input: unsafeCrashWith "TODO"
        , output: unsafeCrashWith "TODO"
        }
  , let
      _n = me "n"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over Compose at t"
        , input: unsafeCrashWith "TODO"
        , output: unsafeCrashWith "TODO"
        }
  , let
      _n = me "n"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over Compose at s"
        , input: unsafeCrashWith "TODO"
        , output: unsafeCrashWith "TODO"
        }
  , let
      _n = me "n"
      _a = me "a"
      _a' = me "a'"
      _A = me "A"
    in
      RS.Rule
        { name: "propagate BoundaryTerm over Top"
        , input:
            topDer { n: _n, a: _a' }
              ( boundaryTermDer { n: _n, a: _a, b: _a' }
                  _A
              )
        , output:
            topDer { n: _n, a: _a }
              _A
        }
  ]

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

example :: Example
example =
  { name: "ISLC-0.2.2"
  , rules
  , tests:
      [ let
          _n = holeCtx
          _m = holeCtx
          _s = holeSub
          _b = holeTerm
          _S = holeSubDer { m: _m, n: _n, a: _s }
          _B = holeTermDer { n: sucCtx _m, a: _b }
        in
          { name: "propagate Sub inside Lam"
          , input:
              subDer { m: _m, n: _n, s: _s, a: lamTerm _b }
                _S
                ( lamDer { n: _m, b: _b }
                    _B
                )
          , output:
              boundaryTermDer { n: _n, a: lamTerm (subTerm ((shiftSub `composeSub` _s) `extendSub` zeroTerm) _b), b: subTerm _s (lamTerm _b) }
                ( lamDer { n: _n, b: subTerm ((shiftSub `composeSub` _s) `extendSub` zeroTerm) _b }
                    ( subDer { m: sucCtx _m, n: sucCtx _n, s: (shiftSub `composeSub` _s) `extendSub` zeroTerm, a: _b }
                        ( extendDer { m: _m, n: sucCtx _n, s: shiftSub `composeSub` _s, a: zeroTerm }
                            ( composeDer { l: _n, m: _n, n: sucCtx _n, s: shiftSub, t: _s }
                                (shiftDer { n: _n })
                                _S
                            )
                            (zeroDer { n: _n })
                        )
                        _B
                    )
                )
          }
      ]
  }

