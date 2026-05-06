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

context = "context"
typ = "type"
term = "term"
name = "name"
judgment = "judgment"

typing = "typing"
nil = "nil"
cons = "cons"
unit = "unit"
arr = "arr"
var = "var"
lam = "lam"
app = "app"

lamD = "Lam"

sequentSystem :: SequentSystem String String
sequentSystem =
  { rules: case _ of
      -- Judgment
      s | s == typing -> makeSequentRule [ context, typ, term ] judgment
      -- Context
      s | s == nil -> makeSequentRule [] context
      s | s == cons -> makeSequentRule [ typ, context ] context
      -- Type
      s | s == unit -> makeSequentRule [] typ
      s | s == arr -> makeSequentRule [ typ, typ ] "Type"
      -- Term
      s | s == var -> makeSequentRule [ name ] term
      s | s == lam -> makeSequentRule [ name, term ] term
      s | s == app -> makeSequentRule [ term, term ] term
      -- Name
      s | Just _ <- String.stripPrefix (String.Pattern "Name:") s -> makeSequentRule [] "Name"
      --   
      s -> unsafeCrashWith $ "Unrecognized sequent label: " <> show s
  , showSequent:
      let
        showSequent = case _ of
          -- Judgment
          Expr sq [ g, t, a ] | sq == judgment -> showSequent g <> " |- " <> showSequent a <> " : " <> showSequent t
          -- Context
          Expr sq [] | sq == nil -> "[]"
          Expr sq [ t, g ] | sq == cons -> showSequent t <> ", " <> showSequent g
          -- Type
          Expr sq [] | sq == unit -> "unit"
          Expr sq [ s, t ] | sq == arr -> "(" <> showSequent s <> " -> " <> showSequent t <> ")"
          -- Term
          Expr sq [ x ] | sq == var -> showSequent x
          Expr sq [ x, b ] | sq == lam -> "lambda " <> showSequent x <> " . " <> showSequent b
          Expr sq [ f, a ] | sq == app -> "(" <> showSequent f <> ") " <> showSequent a
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
          [ makeDerivationRule lamD
              [ typing %% [ cons %% [ mvS "A", mvS "Gamma" ], mvS "B", mvS "b" ] ]
              (typing %% [ mvS "Gamma", arr %% [ mvS "A", mvS "B" ], lam %% [ mvS "x", mvS "b" ] ])
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

