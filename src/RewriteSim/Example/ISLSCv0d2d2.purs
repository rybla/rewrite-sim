module RewriteSim.Example.ISLSCv0d2d2 where

import Prelude hiding (zero)

import Control.Applicative (pure)
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe)
import Data.Unfoldable (none)
import Data.Variant (Variant)
import Data.Variant as Variant
import Debug as Debug
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import RewriteSim ((%))
import RewriteSim as RS
import RewriteSim.Example.Common (Example, Expr, GenericExpr, Rule, me)
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

-- contexts

zeroCtx :: forall x. GenericExpr x
zeroCtx = "zeroCtx" % []

sucCtx :: forall x. GenericExpr x -> GenericExpr x
sucCtx n = "sucCtx" % [ n ]

-- substitutions

shiftSub :: forall x. GenericExpr x
shiftSub = "shift" % []

extendSub :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
extendSub s a = "extend" % [ s, a ]

composeSub :: forall x. GenericExpr x -> GenericExpr x -> GenericExpr x
composeSub s t = "compose" % [ s, t ]

--------------------------------------------------------------------------------
-- Derivation Rules
--------------------------------------------------------------------------------

-- derivation rules for top

topDer :: forall x. GenericExpr x -> GenericExpr x
topDer aDer = "Top" % [ aDer ]

-- derivation rules for terms

zeroDer :: forall x. { n :: GenericExpr x } -> GenericExpr x
zeroDer { n } = "Zero" % [ n ]

lamDer :: forall x. { b :: GenericExpr x, n :: GenericExpr x } -> GenericExpr x -> GenericExpr x
lamDer { n, b } bDer = "Lam" % [ n, b, bDer ]

subDer :: forall x. { a :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
subDer { m, n, s, a } sDer aDer = "Sub" % [ m, n, s, a, sDer, aDer ]

boundaryDer :: forall x. { a :: GenericExpr x, b :: GenericExpr x, n :: GenericExpr x } -> GenericExpr x -> GenericExpr x
boundaryDer { n, a, b } aDer = "Boundary" % [ n, a, b, aDer ]

-- derivation rules for substitutions

shiftDer :: forall x. { n :: GenericExpr x } -> GenericExpr x
shiftDer { n } = "Shift" % [ n ]

extendDer :: forall x. { a :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
extendDer { m, n, s, a } sDer aDer = "Extend" % [ m, n, s, a, sDer, aDer ]

composeDer :: forall x. { l :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x, t :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
composeDer { l, m, n, s, t } sDer tDer = "Compose" % [ l, m, n, s, t, sDer, tDer ]

--------------------------------------------------------------------------------
-- Propagation Rules
--------------------------------------------------------------------------------

rules :: Array Rule
rules =
  [ {- let
    _n = me "n"
    _m = me "m"
    _s = me "s"
    _b = me "b"
    _S = me "S"
    _B = me "B"
  in
    RS.Rule
      { name: "[s] (λ b) ~~> λ [(↑ ∘ s) ∙ zero] b"
      , input:
          subDer { m: _m, n: _n, s: _s, a: lamTerm (_b) }
            (_S)
            ( lamDer { n: _m, b: _b }
                (_B)
            )
      , output:
          boundaryDer { n: _n, a: lamTerm (subTerm ((shiftSub `composeSub` _s) `extendSub` zeroTerm) _b), b: subTerm _s (lamTerm _b) }
            ( lamDer ?a
                ( subDer ?a
                    ( extendDer ?a
                        ( composeDer ?a
                            (shiftDer ?a)
                            (_S)
                        )
                        ?a
                    )
                    (zeroDer ?a)
                )
            )
      } -}
  ]

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
getIndicesOfDer ("Sub" % [ _m, n, s, a, _sDer, _aDer ]) = pure { n: n, a: s `subTerm` a }
getIndicesOfDer ("Boundary" % [ n, _a, b, _aDer ]) = pure { n: n, a: b }
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
  subDer { m: sucCtx (sucCtx n), n: sucCtx (sucCtx (sucCtx n)), s: shiftSub, a: shiftSub `subTerm` zeroTerm }
    (shiftDer { n: sucCtx (sucCtx n) })
    (oneDer' { n: n })

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

example :: Example
example =
  { name: "ISLC-0.2.2"
  , rules
  , tests: []
  }

