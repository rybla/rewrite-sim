module RewriteSim.Example.DerivationsEx1 where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array as Array
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (GenericExpr(..), MetaVar(..))
import RewriteSim.Example.Library.Derivations (DerivationRuleContext, DerivationRuleError, DerivationSystem, Sequent, SequentSystem, makeDerivationRule, makeSequentRule, (%%))

mvS :: forall m. Monad m => String -> m (Sequent String)
mvS label = pure $ MetaExpr (MetaVar { label, index: 0 })

sequentSystem :: SequentSystem String String
sequentSystem =
  { rules: case _ of
      -- Judgment
      "typing" -> makeSequentRule [ "Context", "Type", "Term" ] "Judgment"
      -- Context
      "nil" -> makeSequentRule [] "Context"
      "cons" -> makeSequentRule [ "Type", "Context" ] "Context"
      -- Type
      "unit" -> makeSequentRule [] "Type"
      "arr" -> makeSequentRule [ "Type", "Type" ] "Type"
      -- Term
      "var" -> makeSequentRule [ "Name" ] "Term"
      "lam" -> makeSequentRule [ "Name", "Term" ] "Term"
      "app" -> makeSequentRule [ "Term", "Term" ] "Term"
      -- Name
      s | Just _ <- String.stripPrefix (String.Pattern "Name:") s -> makeSequentRule [] "Name"
      --   
      s -> unsafeCrashWith $ "Unrecognized sequent label: " <> show s
  , showSequent:
      let
        showSequent = case _ of
          -- Judgment
          Expr "typing" [ g, t, a ] -> showSequent g <> " |- " <> showSequent a <> " : " <> showSequent t
          -- Context
          Expr "nil" [] -> "[]"
          Expr "cons" [ t, g ] -> showSequent t <> ", " <> showSequent g
          -- Type
          Expr "unit" [] -> "unit"
          Expr "arr" [ s, t ] -> "(" <> showSequent s <> " -> " <> showSequent t <> ")"
          -- Term
          Expr "var" [ x ] -> showSequent x
          Expr "lam" [ x, b ] -> "lambda " <> showSequent x <> " . " <> showSequent b
          Expr "app" [ f, a ] -> "(" <> showSequent f <> ") " <> showSequent a
          -- Name
          Expr s [] | Just x <- String.stripPrefix (String.Pattern "name:") s -> x
          --
          e -> show e
      in
        showSequent
  }

runDerivationRuleM
  :: forall a
   . ReaderT (DerivationRuleContext String String) (ExceptT (DerivationRuleError String) Identity) a
  -> a
runDerivationRuleM m = m
  # flip runReaderT { sequentSystem }
  # runExceptT
  # unwrap
  # either (\error -> unsafeCrashWith $ "[runDerivationRuleM] At derivation label " <> error.derivationLabel <> ": " <> error.message) identity

derivationSystem :: DerivationSystem String String
derivationSystem =
  { rules:
      let
        rules = map runDerivationRuleM
          [ makeDerivationRule "Lam"
              [ "typing" %% [ "cons" %% [ mvS "A", mvS "Gamma" ], mvS "B", mvS "b" ] ]
              ("typing" %% [ mvS "Gamma", "arr" %% [ mvS "A", mvS "B" ], "lam" %% [ mvS "x", mvS "b" ] ])
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
          Expr "Lam" [ x, b ] -> "lambda " <> showDerivation x <> " . " <> showDerivation b
          e -> show e
      in
        showDerivation
  }

