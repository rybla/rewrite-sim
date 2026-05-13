module RewriteSim.Example.DerivationsEx1 where

import Prelude hiding (zero)

import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (class IsExprLabel, prettyExpr)
import RewriteSim as RS
import RewriteSim.Example.Library.Derivations (DerivationRuleCtx, DerivationRuleError, DerivationSystem, Sequent, SequentSystem, makeDerivationRule, makeSequentRule, (%), (%%))
import RewriteSim.Logging (class MonadLogger, log)
import RewriteSim.Pretty (class Pretty, pretty)
import RewriteSim.Utilities (throw)

--------------------------------------------------------------------------------
-- Labels
--------------------------------------------------------------------------------

-- Sorts

newtype SortLabel = SortLabel String

derive newtype instance Show SortLabel
derive newtype instance Pretty SortLabel
derive newtype instance Eq SortLabel

contextSort = SortLabel "context"
typeSort = SortLabel "type"
varSort = SortLabel "var"
termSort = SortLabel "term"
judgmentSort = SortLabel "judgment"

-- Sequents

newtype SequentLabel = SequentLabel String

derive newtype instance Show SequentLabel
derive newtype instance Pretty SequentLabel
derive newtype instance Eq SequentLabel
derive newtype instance Ord SequentLabel

typingS = SequentLabel "typing"
typingVarS = SequentLabel "typingVar"
nilS = SequentLabel "nil"
consS = SequentLabel "cons"
unitS = SequentLabel "unit"
arrS = SequentLabel "arr"
zeroS = SequentLabel "zero"
sucS = SequentLabel "suc"
varS = SequentLabel "var"
lamS = SequentLabel "lam"
appS = SequentLabel "app"

typing gamma alpha a = typingS %% [ gamma, alpha, a ]
typingVar gamma alpha x = typingVarS %% [ gamma, alpha, x ]
nil = nilS %% []
cons alpha gamma = consS %% [ alpha, gamma ]
zero = zeroS %% []
suc x = sucS %% [ x ]
unit = unitS %% []
arr a b = arrS %% [ a, b ]
var x = varS %% [ x ]
lam b = lamS %% [ b ]
app f a = appS %% [ f, a ]

instance IsExprLabel SequentLabel where
  expectedKids _ = Nothing

  -- Judgment
  prettyExpr' s [ gamma, alpha, a ] | s == typingS = prettyExpr gamma <> " |- " <> prettyExpr a <> " : " <> prettyExpr alpha
  prettyExpr' s [ gamma, alpha, x ] | s == typingVarS = prettyExpr gamma <> " |-_var " <> prettyExpr x <> " : " <> prettyExpr alpha
  -- Ctx
  prettyExpr' s [] | s == nilS = "[]"
  prettyExpr' s [ alpha, gamma ] | s == consS = prettyExpr alpha <> ", " <> prettyExpr gamma
  -- Type
  prettyExpr' s [] | s == unitS = "unit"
  prettyExpr' s [ alpha, beta ] | s == arrS = "(" <> prettyExpr alpha <> " -> " <> prettyExpr beta <> ")"
  -- Var
  prettyExpr' s [] | s == zeroS = "z"
  prettyExpr' s [ x ] | s == sucS = "s" <> prettyExpr x
  -- Term
  prettyExpr' s [ x ] | s == varS = "v" <> prettyExpr x
  prettyExpr' s [ b ] | s == lamS = "λ " <> prettyExpr b
  prettyExpr' s [ f, a ] | s == appS = "(" <> prettyExpr f <> ") " <> prettyExpr a
  prettyExpr' s _ = unsafeCrashWith $ "Unrecognized sequent label: " <> show s

-- Derivations

newtype DerivationLabel = DerivationLabel String

derive newtype instance Show DerivationLabel
derive newtype instance Pretty DerivationLabel
derive newtype instance Eq DerivationLabel
derive newtype instance Ord DerivationLabel

nilD = DerivationLabel "nil"
consD = DerivationLabel "cons"
unitD = DerivationLabel "unit"
arrD = DerivationLabel "arr"
zeroD = DerivationLabel "zero"
sucD = DerivationLabel "suc"
varD = DerivationLabel "var"
lamD = DerivationLabel "lam"
appD = DerivationLabel "app"

nil_ = nilD % []
cons_ alpha gamma = consD % [ alpha, gamma ]
unit_ = unitD % []
arr_ a b = arrD % [ a, b ]
zero_ = zeroD % []
suc_ x = sucD % [ x ]
var_ x = varD % [ x ]
lam_ b = lamD % [ b ]
app_ f a = appD % [ f, a ]

instance IsExprLabel DerivationLabel where
  expectedKids _ = Nothing

  prettyExpr' d [] | d == zeroD = "z"
  prettyExpr' d [ x ] | d == sucD = "s" <> prettyExpr x
  prettyExpr' d [ x ] | d == varD = "v" <> prettyExpr x
  prettyExpr' d [ b ] | d == lamD = "λ " <> prettyExpr b
  prettyExpr' d [ f, a ] | d == appD = "(" <> prettyExpr f <> ") " <> prettyExpr a
  prettyExpr' d _ = unsafeCrashWith $ "Unrecognized derivation label: " <> show d

-- | sequent metavariable
me :: forall m. Monad m => String -> m (Sequent SequentLabel)
me label = pure $ RS.me label

--------------------------------------------------------------------------------

sequentSystem :: SequentSystem SortLabel SequentLabel
sequentSystem =
  { rules: case _ of
      -- Judgment
      s | s == typingS -> makeSequentRule [ contextSort, typeSort, termSort ] judgmentSort
      s | s == typingVarS -> makeSequentRule [ contextSort, typeSort, varSort ] judgmentSort
      -- Ctx
      s | s == nilS -> makeSequentRule [] contextSort
      s | s == consS -> makeSequentRule [ typeSort, contextSort ] contextSort
      -- Type
      s | s == unitS -> makeSequentRule [] typeSort
      s | s == arrS -> makeSequentRule [ typeSort, typeSort ] typeSort
      -- Var
      s | s == zeroS -> makeSequentRule [] varSort
      s | s == sucS -> makeSequentRule [ varSort ] varSort
      -- Term
      s | s == varS -> makeSequentRule [ varSort ] termSort
      s | s == lamS -> makeSequentRule [ termSort ] termSort
      s | s == appS -> makeSequentRule [ termSort, termSort ] termSort
      --   
      s -> unsafeCrashWith $ "Unrecognized sequent label: " <> pretty s
  }

makeDerivationSystem
  :: forall m
   . MonadLogger m
  => MonadThrow Error m
  => m (DerivationSystem SequentLabel DerivationLabel)
makeDerivationSystem = do
  log "makeDerivationSystem" Nothing
  let
    runDerivationRuleM
      :: forall a
       . ReaderT (DerivationRuleCtx SortLabel SequentLabel) (ExceptT (DerivationRuleError DerivationLabel) m) a
      -> m a
    runDerivationRuleM m = m
      # flip runReaderT { sequentSystem }
      # runExceptT
      # bindFlipped (either (\error -> throw $ "Error in derivation rule for derivation label " <> pretty error.derivationLabel <> ": " <> error.message) pure)

  rules <-
    traverse runDerivationRuleM
      [ makeDerivationRule zeroD
          []
          (typingVar (cons (me "alpha") (me "gamma")) (me "alpha") zero)

      , makeDerivationRule sucD
          [ typingVar (me "gamma") (me "alpha") (me "x") ]
          (typingVar (cons (me "beta") (me "gamma")) (me "alpha") (suc (me "x")))

      , makeDerivationRule varD
          [ typingVar (me "gamma") (me "alpha") (me "x") ]
          (typing (me "gamma") (me "alpha") (var (me "x")))

      , makeDerivationRule lamD
          [ typing (cons (me "alpha") (me "gamma")) (me "beta") (me "b") ]
          (typing (me "gamma") (arr (me "alpha") (me "beta")) (lam (me "b")))

      , makeDerivationRule appD
          [ typing (me "gamma") (arr (me "alpha") (me "beta")) (me "f")
          , typing (me "gamma") (me "alpha") (me "a")
          ]
          (typing (me "gamma") (me "beta") (app (me "f") (me "a")))
      ]

  pure
    { rules: \d ->
        case rules # Array.find (\(d' /\ _) -> d == d') # map snd of
          Just rule -> rule
          Nothing -> unsafeCrashWith $ "Unrecognized derivation label: " <> pretty d
    }
