module RewriteSim.Example.DerivationsEx1 where

import Prelude

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
import RewriteSim.Utilities (throw)

--------------------------------------------------------------------------------
-- Labels
--------------------------------------------------------------------------------

-- Sorts

newtype SortLabel = SortLabel String

derive newtype instance Show SortLabel
derive newtype instance Eq SortLabel

contextSort = SortLabel "context"
typeSort = SortLabel "type"
termSort = SortLabel "term"
nameSort = SortLabel "name"
judgmentSort = SortLabel "judgment"

-- Sequents

newtype SequentLabel = SequentLabel String

derive newtype instance Show SequentLabel
derive newtype instance Eq SequentLabel

typingS = SequentLabel "typing"
nilS = SequentLabel "nil"
consS = SequentLabel "cons"
unitS = SequentLabel "unit"
arrS = SequentLabel "arr"
varS = SequentLabel "var"
lamS = SequentLabel "lam"
appS = SequentLabel "app"

toNameS s = SequentLabel $ "Name:" <> s
fromNameS (SequentLabel s) = String.stripPrefix (String.Pattern "Name:") s

typing gamma alpha a = typingS %% [ gamma, alpha a ]
nil = nilS %% []
cons alpha gamma = consS %% [ alpha, gamma ]
unit = unitS %% []
arr a b = arrS %% [ a, b ]
var x = varS %% [ x ]
lam x b = lamS %% [ x, b ]
app f a = appS %% [ f, a ]

-- Derivations

newtype DerivationLabel = DerivationLabel String

derive newtype instance Show DerivationLabel
derive newtype instance Eq DerivationLabel

nilD = DerivationLabel "nil"
consD = DerivationLabel "cons"

unitD = DerivationLabel "unit"
arrD = DerivationLabel "arr"

varD = DerivationLabel "var"
lamD = DerivationLabel "lam"
appD = DerivationLabel "app"

toNameD s = DerivationLabel $ "name:" <> s
fromNameD (DerivationLabel s) = String.stripPrefix (String.Pattern s) s

nil_ = nilD % []
cons_ alpha gamma = consD % [ alpha, gamma ]

unit_ = unitD % []
arr_ a b = arrD % [ a, b ]

var_ x = varD % [ x ]
lam_ x b = lamD % [ x, b ]
app_ f a = appD % [ f, a ]

name_ s = toNameD s % []

-- | sequent metavariable
mv :: forall m. Monad m => String -> m (Sequent SequentLabel)
mv label = pure $ MetaExpr (MetaVar { label, index: -1 })

--------------------------------------------------------------------------------

sequentSystem :: SequentSystem SortLabel SequentLabel
sequentSystem =
  { rules: case _ of
      -- Judgment
      s | s == typingS -> makeSequentRule [ contextSort, typeSort, termSort ] judgmentSort
      -- Ctx
      s | s == nilS -> makeSequentRule [] contextSort
      s | s == consS -> makeSequentRule [ typeSort, contextSort ] contextSort
      -- Type
      s | s == unitS -> makeSequentRule [] typeSort
      s | s == arrS -> makeSequentRule [ typeSort, typeSort ] typeSort
      -- Term
      s | s == varS -> makeSequentRule [ nameSort ] termSort
      s | s == lamS -> makeSequentRule [ nameSort, termSort ] termSort
      s | s == appS -> makeSequentRule [ termSort, termSort ] termSort
      -- Name
      SequentLabel s | Just _ <- String.stripPrefix (String.Pattern "Name:") s -> makeSequentRule [] nameSort
      --   
      s -> unsafeCrashWith $ "Unrecognized sequent label: " <> show s
  , showSequent:
      let
        showSequent :: Sequent SequentLabel -> String
        showSequent = case _ of
          -- Judgment
          Expr s [ g, t, a ] | s == typingS -> showSequent g <> " |- " <> showSequent a <> " : " <> showSequent t
          -- Ctx
          Expr s [] | s == nilS -> "[]"
          Expr s [ t, g ] | s == consS -> showSequent t <> ", " <> showSequent g
          -- Type
          Expr s [] | s == unitS -> "unit"
          Expr s [ a, b ] | s == arrS -> "(" <> showSequent a <> " -> " <> showSequent b <> ")"
          -- Term
          Expr s [ x ] | s == varS -> showSequent x
          Expr s [ x, b ] | s == lamS -> "lambda " <> showSequent x <> " . " <> showSequent b
          Expr s [ f, a ] | s == appS -> "(" <> showSequent f <> ") " <> showSequent a
          -- Name
          Expr s [] | Just x <- fromNameS s -> x
          --
          e -> show e
      in
        showSequent
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
      # bindFlipped (either (\error -> throw $ "Error in derivation rule for derivation label " <> show error.derivationLabel <> ": " <> error.message) pure)

  rules <-
    traverse runDerivationRuleM
      [ makeDerivationRule lamD
          [ typingS %% [ consS %% [ mv "A", mv "Gamma" ], mv "B", mv "b" ] ]
          (typingS %% [ mv "Gamma", arrS %% [ mv "A", mv "B" ], lamS %% [ mv "x", mv "b" ] ])
      ]
  pure
    { rules: \d ->
        case rules # Array.find (\(d' /\ _) -> d == d') # map snd of
          Just rule -> rule
          -- name rule family
          Nothing | Just s <- fromNameD d ->
            { hypotheses: []
            , conclusion: Expr (toNameS s) []
            }
          Nothing -> unsafeCrashWith $ "Unrecognized derivation label: " <> show d
    , showDerivation:
        let
          showDerivation = case _ of
            Expr d [ x, b ] | d == lamD -> "lambda " <> showDerivation x <> " . " <> showDerivation b
            e -> show e
        in
          showDerivation
    }

