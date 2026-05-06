module RewriteSim.Example.DerivationsEx1 (sequentSystem, derivationSystem) where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array as Array
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (GenericExpr(..), MetaVar(..))
import RewriteSim.Example.Library.Derivations (DerivationRuleContext, DerivationRuleError, DerivationSystem, Sequent, SequentSystem, makeDerivationRule, makeSequentRule, (%%))
import RewriteSim.Utilities (runIdentity)

--------------------------------------------------------------------------------
-- Labels
--------------------------------------------------------------------------------

-- sorts
contextSort = "context" :: String
typeSort = "type" :: String
termSort = "term" :: String
nameSort = "name" :: String
judgmentSort = "judgment" :: String

-- sequents
typingS = "typing" :: String
nilS = "nil" :: String
consS = "cons" :: String
unitS = "unit" :: String
arrS = "arr" :: String
varS = "var" :: String
lamS = "lam" :: String
appS = "app" :: String

-- derivations
lamD = "Lam" :: String

-- sequent metavariable
mvS :: forall m. Monad m => String -> m (Sequent String)
mvS label = pure $ MetaExpr (MetaVar { label, index: -1 })

--------------------------------------------------------------------------------

sequentSystem :: SequentSystem String String
sequentSystem =
  { rules: case _ of
      -- Judgment
      s | s == typingS -> makeSequentRule [ contextSort, typeSort, termSort ] judgmentSort
      -- Context
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
      s | Just _ <- String.stripPrefix (String.Pattern "Name:") s -> makeSequentRule [] "Name"
      --   
      s -> unsafeCrashWith $ "Unrecognized sequent label: " <> show s
  , showSequent:
      let
        showSequent = case _ of
          -- Judgment
          Expr sq [ g, t, a ] | sq == judgmentSort -> showSequent g <> " |- " <> showSequent a <> " : " <> showSequent t
          -- Context
          Expr sq [] | sq == nilS -> "[]"
          Expr sq [ t, g ] | sq == consS -> showSequent t <> ", " <> showSequent g
          -- Type
          Expr sq [] | sq == unitS -> "unit"
          Expr sq [ s, t ] | sq == arrS -> "(" <> showSequent s <> " -> " <> showSequent t <> ")"
          -- Term
          Expr sq [ x ] | sq == varS -> showSequent x
          Expr sq [ x, b ] | sq == lamS -> "lambda " <> showSequent x <> " . " <> showSequent b
          Expr sq [ f, a ] | sq == appS -> "(" <> showSequent f <> ") " <> showSequent a
          -- Name
          Expr s [] | Just x <- String.stripPrefix (String.Pattern "name:") s -> x
          --
          e -> show e
      in
        showSequent
  }

-- TODO: other derivation rules
derivationSystem :: DerivationSystem String String
derivationSystem =
  { rules:
      let
        rules = map runDerivationRuleM
          [ makeDerivationRule lamD
              [ typingS %% [ consS %% [ mvS "A", mvS "Gamma" ], mvS "B", mvS "b" ] ]
              (typingS %% [ mvS "Gamma", arrS %% [ mvS "A", mvS "B" ], lamS %% [ mvS "x", mvS "b" ] ])
          ]
      in
        \d ->
          case
            rules # Array.findMap
              ( \(d' /\ rule) -> do
                  guard (d == d')
                  pure rule
              )
            of
            Nothing -> unsafeCrashWith $ "Unrecognized derivation label: " <> show d
            Just rule -> rule
  , showDerivation:
      let
        showDerivation = case _ of
          Expr d [ x, b ] | d == lamD -> "lambda " <> showDerivation x <> " . " <> showDerivation b
          e -> show e
      in
        showDerivation
  }

  where
  runDerivationRuleM
    :: forall a
     . ReaderT (DerivationRuleContext String String) (ExceptT (DerivationRuleError String) Identity) a
    -> a
  runDerivationRuleM m = m
    # flip runReaderT { sequentSystem }
    # runExceptT
    # runIdentity
    # either (\error -> unsafeCrashWith $ "Error in derivation rule for derivation label " <> error.derivationLabel <> ": " <> error.message) identity
