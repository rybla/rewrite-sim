module Test.RewriteSim.Example.DerivationsEx1 where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import RewriteSim (prettyExpr)
import RewriteSim.Example.DerivationsEx1 (DerivationLabel, SequentLabel, SortLabel, app_, lam_, makeDerivationSystem, sequentSystem, suc_, var_, zero_)
import RewriteSim.Example.Library.Derivations (DerivationSystem, DerivingCtx, DerivingEnv, DerivingError, newDerivingCtx, newDerivingEnv)
import RewriteSim.Logging (LoggerT, log, log_)
import RewriteSim.Utilities (runExceptThrow)
import Test.RewriteSim.Common (LoggerSpecT, hoistLoggerSpec)
import Test.Spec (SpecT, beforeAll, describe, it)

--------------------------------------------------------------------------------

type DerivationTestInput =
  { derivationSystem :: DerivationSystem SequentLabel DerivationLabel
  }

-- TODO: Implement the ability to assert that two expressions are equivalent up to renaming metavariables, which is derived from doing a unification and checking that each of the unification substitutions is merely a substitution of one metavariable for another. 

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "DerivationsEx1"
    $ hoistLoggerSpec "logs/test/DerivationsEx1.log"
    $ beforeAll setup
        do
          let
            runDerivingTest
              :: forall m a
               . MonadThrow Error m
              => ReaderT (DerivingCtx SortLabel SequentLabel DerivationLabel) (StateT (DerivingEnv SequentLabel DerivationLabel) (ExceptT DerivingError m)) a
              -> DerivationTestInput
              -> m a
            runDerivingTest m ctx = do
              m
                # flip runReaderT (newDerivingCtx { sequentSystem, derivationSystem: ctx.derivationSystem })
                # flip evalStateT (newDerivingEnv {})
                # runExceptThrow (\error -> "Deriving error: " <> error.message)

          it "rules" $ runDerivingTest $ do
            pure unit

          describe "derivations" do
            let
              makeTest :: String -> _ -> LoggerSpecT Aff _ Aff Unit
              makeTest testName m = it testName $ runDerivingTest do
                log_ $ "test: " <> testName
                derivation /\ sequent <- m
                log ("test: " <> testName) $ pure $ "derivation = " <> prettyExpr derivation
                log ("test: " <> testName) $ pure $ "sequent = " <> prettyExpr sequent
                pure unit

            makeTest "vz" $ var_ zero_
            makeTest "lam" $ lam_ (var_ zero_)
            makeTest "app" $ app_ (var_ (suc_ zero_)) (var_ zero_)

            pure unit

          pure unit

  where

  setup :: LoggerT Aff DerivationTestInput
  setup = do
    derivationSystem <- makeDerivationSystem
    pure { derivationSystem }