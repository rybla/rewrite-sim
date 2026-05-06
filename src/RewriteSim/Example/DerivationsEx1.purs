module RewriteSim.Example.DerivationsEx1 where

import Prelude hiding (zero)

import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Exception (Error)
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (GenericExpr(..), MetaVar(..))
import RewriteSim.Example.Library.Derivations (DerivationRuleCtx, DerivationRuleError, DerivationSystem, Sequent, SequentSystem, makeDerivationRule, makeSequentRule, (%), (%%))
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

-- Derivations

newtype DerivationLabel = DerivationLabel String

derive newtype instance Show DerivationLabel
derive newtype instance Pretty DerivationLabel
derive newtype instance Eq DerivationLabel

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

-- | sequent metavariable
mv :: forall m. Monad m => String -> m (Sequent SequentLabel)
mv label = pure $ MetaExpr (MetaVar { label, index: -1 })

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
  , prettySequent:
      let
        prettySequent :: Sequent SequentLabel -> String
        prettySequent = case _ of
          -- Judgment
          Expr s [ g, t, a ] | s == typingS -> prettySequent g <> " |- " <> prettySequent a <> " : " <> prettySequent t
          -- Ctx
          Expr s [] | s == nilS -> "[]"
          Expr s [ t, g ] | s == consS -> prettySequent t <> ", " <> prettySequent g
          -- Type
          Expr s [] | s == unitS -> "unit"
          Expr s [ a, b ] | s == arrS -> "(" <> prettySequent a <> " -> " <> prettySequent b <> ")"
          -- Term
          Expr s [ x ] | s == varS -> prettySequent x
          Expr s [ b ] | s == lamS -> "λ " <> prettySequent b
          Expr s [ f, a ] | s == appS -> "(" <> prettySequent f <> ") " <> prettySequent a
          --
          e -> pretty e
      in
        prettySequent
  }

-- TODO: other derivation rules
makeDerivationSystem :: forall m. MonadThrow Error m => m (DerivationSystem SequentLabel DerivationLabel)
makeDerivationSystem = do
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
          (typingVar (cons (mv "alpha") (mv "gamma")) (mv "alpha") zero)
      , makeDerivationRule sucD
          [ typingVar (mv "gamma") (mv "alpha") (mv "x") ]
          (typingVar (cons (mv "beta") (mv "gamma")) (mv "alpha") (suc (mv "x")))
      , makeDerivationRule varD
          [ typingVar (mv "gamma") (mv "alpha") (mv "x") ]
          (typing (mv "gamma") (mv "alpha") (var (mv "x")))
      , makeDerivationRule lamD
          [ typing (cons (mv "alpha") (mv "gamma")) (mv "beta") (mv "b") ]
          (typing (mv "gamma") (arr (mv "alpha") (mv "beta")) (lam (mv "b")))
      ]
  pure
    { rules: \d ->
        case rules # Array.find (\(d' /\ _) -> d == d') # map snd of
          Just rule -> rule
          Nothing -> unsafeCrashWith $ "Unrecognized derivation label: " <> pretty d
    , prettyDerivation:
        let
          prettyDerivation = case _ of
            Expr d [] | d == zeroD -> "z"
            Expr d [ x ] | d == sucD -> "s" <> prettyDerivation x
            Expr d [ x ] | d == varD -> "v" <> prettyDerivation x
            Expr d [ b ] | d == lamD -> "λ " <> prettyDerivation b
            e -> pretty e
        in
          prettyDerivation
    }

