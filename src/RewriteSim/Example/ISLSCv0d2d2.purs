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
import RewriteSim.Example.Common (Example, GenericExpr, Expr)
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

zeroTermDer :: forall x. { n :: GenericExpr x } -> GenericExpr x
zeroTermDer { n } = "Zero" % [ n ]

lamTermDer :: forall x. { b :: GenericExpr x, n :: GenericExpr x } -> GenericExpr x -> GenericExpr x
lamTermDer { n, b } bDer = "Lam" % [ n, b, bDer ]

subTermDer :: forall x. { a :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
subTermDer { m, n, s, a } sDer aDer = "Sub" % [ m, n, s, a, sDer, aDer ]

boundaryTermDer :: forall x. { a :: GenericExpr x, b :: GenericExpr x, n :: GenericExpr x } -> GenericExpr x -> GenericExpr x
boundaryTermDer { n, a, b } aDer = "Boundary" % [ n, a, b, aDer ]

-- derivation rules for substitutions

shiftSubDer :: forall x. { n :: GenericExpr x } -> GenericExpr x
shiftSubDer { n } = "Shift" % [ n ]

extendSubDer :: forall x. { a :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
extendSubDer { m, n, s, a } sDer aDer = "Extend" % [ m, n, s, a, sDer, aDer ]

composeSubDer :: forall x. { l :: GenericExpr x, m :: GenericExpr x, n :: GenericExpr x, s :: GenericExpr x, t :: GenericExpr x } -> GenericExpr x -> GenericExpr x -> GenericExpr x
composeSubDer { l, m, n, s, t } sDer tDer = "Compose" % [ l, m, n, s, t, sDer, tDer ]

--------------------------------------------------------------------------------
-- meta-functions
--------------------------------------------------------------------------------

type ErrorRow x =
  ( invalid :: { sort :: String, expr :: GenericExpr x }
  , meta :: { sort :: String, var :: x }
  )

getIndicesOfTermDer
  :: forall m x
   . MonadError (Variant (ErrorRow x)) m
  => GenericExpr x
  -> m { n :: GenericExpr x, a :: GenericExpr x }
getIndicesOfTermDer (RS.MetaExpr var) = throwError $ Variant.inj (Proxy @"meta") { sort: "TermDer", var }
getIndicesOfTermDer ("Zero" % [ n ]) = pure { n: sucCtx n, a: zeroTerm }
getIndicesOfTermDer ("Lam" % [ n, b, _bDer ]) = pure { n: n, a: lamTerm b }
getIndicesOfTermDer ("Sub" % [ _m, n, s, a, _sDer, _aDer ]) = pure { n: n, a: subTerm s a }
getIndicesOfTermDer ("Boundary" % [ n, _a, b, _aDer ]) = pure { n: n, a: b }
getIndicesOfTermDer expr = throwError $ Variant.inj (Proxy @"invalid") { sort: "TermDer", expr }

--------------------------------------------------------------------------------
-- Example Derivations
--------------------------------------------------------------------------------

shiftTermDerFn
  :: forall m x
   . MonadError (Variant (ErrorRow x)) m
  => Show x
  => GenericExpr x
  -> m (GenericExpr x)
shiftTermDerFn termDer = do
  { n, a } <- getIndicesOfTermDer termDer
  pure $
    subTermDer { m: n, n: sucCtx n, s: shiftSub, a: a }
      (shiftSubDer { n: n })
      termDer

oneTermDer
  :: forall m @x
   . MonadError (Variant (ErrorRow x)) m
  => Show x
  => { n :: GenericExpr x }
  -> m (GenericExpr x)
oneTermDer { n } = shiftTermDerFn (zeroTermDer { n: n })

twoTermDer
  :: forall m @x
   . MonadError (Variant (ErrorRow x)) m
  => Show x
  => { n :: GenericExpr x }
  -> m (GenericExpr x)
twoTermDer { n } = shiftTermDerFn =<< oneTermDer { n: n }

oneTermDer' :: forall x. { n :: GenericExpr x } -> GenericExpr x
oneTermDer' { n } =
  subTermDer { m: sucCtx n, n: sucCtx (sucCtx n), s: shiftSub, a: zeroTerm }
    (shiftSubDer { n: sucCtx n })
    (zeroTermDer { n: n })

twoTermDer' :: forall x. { n :: GenericExpr x } -> GenericExpr x
twoTermDer' { n } =
  subTermDer { m: sucCtx (sucCtx n), n: sucCtx (sucCtx (sucCtx n)), s: shiftSub, a: subTerm shiftSub zeroTerm }
    (shiftSubDer { n: sucCtx (sucCtx n) })
    (oneTermDer' { n: n })

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

example :: Example
example =
  { name: "ISLC-0.2.2"
  , rules: []
  , tests: []
  }

