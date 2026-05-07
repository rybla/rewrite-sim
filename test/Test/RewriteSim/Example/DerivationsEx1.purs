module Test.RewriteSim.Example.DerivationsEx1 where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Node.FS.Sync (rm)
import Node.FS.Sync as FS
import RewriteSim.Example.DerivationsEx1 (DerivationLabel, SequentLabel, SortLabel, app_, lam_, makeDerivationSystem, sequentSystem, suc_, var_, zero_)
import RewriteSim.Example.Library.Derivations (DerivationSystem, DerivingCtx, DerivingEnv, DerivingError, newDerivingCtx, newDerivingEnv)
import RewriteSim.Logging (LoggerT, log_, runLoggerT)
import RewriteSim.Utilities (runExceptThrow)
import Test.Spec (SpecT, beforeAll, describe, hoistSpec, it)

--------------------------------------------------------------------------------

logFilepath :: String
logFilepath = "logs/test/DerivationsEx1.log"

runLoggerT' :: forall m a. MonadEffect m => LoggerT m a -> m a
runLoggerT' m = do
  runLoggerT m { filepath: logFilepath }

--------------------------------------------------------------------------------

spec :: SpecT Aff Unit Aff Unit
spec = hoistSpec runLoggerT' (\_ -> runLoggerT') spec'

type DerivationTestInput =
  { derivationSystem :: DerivationSystem SequentLabel DerivationLabel
  }

spec' :: SpecT (LoggerT Aff) Unit (LoggerT Aff) Unit
spec' =
  describe "DerivationsEx1" do
    beforeAll
      ( do
          whenM (FS.exists logFilepath # liftEffect) do
            rm logFilepath # liftEffect
          derivationSystem <- makeDerivationSystem
          pure { derivationSystem }
      )
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
            makeTest testName m = it testName $ runDerivingTest do
              log_ $ "test: " <> testName
              ctx <- ask
              derivation /\ sequent <- m
              Console.log $ "derivation = " <> ctx.derivationSystem.prettyDerivation derivation
              Console.log $ "sequent = " <> ctx.sequentSystem.prettySequent sequent
              pure unit

          makeTest "vz" $ var_ zero_ -- TODO: why doesn't gamma get inferred to be a cons? 
          makeTest "lam" $ lam_ (var_ zero_)
          makeTest "app" $ app_ (var_ (suc_ zero_)) (var_ zero_) -- TODO: why doesn't gamma get inferred to be a double cons?
