module RewriteSim.Example.DerivationsEx1 where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Options.Applicative.Internal.Utils (startsWith)
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (GenericExpr(..), MetaVar(..))
import RewriteSim.Example.Library.Derivations (DerivationRuleContext, DerivationRuleError, DerivationSystem, Sequent, SequentSystem, makeDerivationRule, makeSequentRule, (%%))

mvS :: forall m. Monad m => String -> m (Sequent String)
mvS label = pure $ MetaExpr (MetaVar { label, index: 0 })

sequentSystem :: SequentSystem String String
sequentSystem =
  { rules: case _ of
      -- Judgment
      "Typing" -> makeSequentRule [ "Context", "Type", "Term" ] "Judgment"
      -- Context
      "Nil" -> makeSequentRule [] "Context"
      "Cons" -> makeSequentRule [ "Type", "Context" ] "Context"
      -- Type
      "Unit" -> makeSequentRule [] "Type"
      "Arr" -> makeSequentRule [ "Type", "Type" ] "Type"
      -- Term
      "Var" -> makeSequentRule [ "Name" ] "Term"
      "Lam" -> makeSequentRule [ "Name", "Term" ] "Term"
      "App" -> makeSequentRule [ "Term", "Term" ] "Term"
      -- Name
      s | Just _ <- String.stripPrefix (String.Pattern "Name:") s -> makeSequentRule [] "Name"
      --   
      s -> unsafeCrashWith $ "Unrecognized sequent label: " <> show s
  , showSequent:
      let
        showSequent = case _ of
          -- Judgment
          Expr "Typing" [ g, t, a ] -> showSequent g <> " |- " <> showSequent a <> " : " <> showSequent t
          -- Context
          Expr "Nil" [] -> "[]"
          Expr "Cons" [ t, g ] -> showSequent t <> ", " <> showSequent g
          -- Type
          Expr "Unit" [] -> "Unit"
          Expr "Arr" [ s, t ] -> "(" <> showSequent s <> " -> " <> showSequent t <> ")"
          -- Term
          Expr "Var" [ x ] -> showSequent x
          Expr "Lam" [ x, b ] -> showSequent x <> " => " <> showSequent b
          Expr "App" [ f, a ] -> "(" <> showSequent f <> ") " <> showSequent a
          -- Name
          Expr s [] | Just x <- String.stripPrefix (String.Pattern "Name:") s -> x
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
  # either (\error -> unsafeCrashWith $ "[runDerivationRuleM] Derivation label was " <> error.derivationLabel <> " and the error was " <> error.message) identity

derivationSystem :: DerivationSystem String String
derivationSystem =
  { rules: case _ of
      "LamD" -> runDerivationRuleM $ makeDerivationRule "LamD"
        [ "Typing" %% [ "Cons" %% [ mvS "Gamma", mvS "A" ], mvS "B", mvS "b" ] ]
        ("Typing" %% [ mvS "Gamma", "Arrow" %% [ mvS "A", mvS "B" ], "Lam" %% [ mvS "x", mvS "b" ] ])
      -- 
      d -> unsafeCrashWith $ "Unrecognized derivation label: " <> show d
  , showDerivation:
      let
        showDerivation = case _ of
          Expr "LamD" [ bD ] -> "lambda " <> showDerivation bD
          _ -> ""
      in
        showDerivation
  }
