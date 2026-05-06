module Test.RewriteSim.Example.DerivationsEx1 where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import RewriteSim.Example.DerivationsEx1 (DerivationLabel, SequentLabel, SortLabel, app_, lam_, makeDerivationSystem, sequentSystem, suc_, var_, zero_)
import RewriteSim.Example.Library.Derivations (DerivationSystem, DerivingCtx, DerivingEnv, DerivingError, newDerivingCtx, newDerivingEnv)
import RewriteSim.Utilities (runExceptThrow)
import Test.Spec (SpecT, beforeAll, describe, it)

type DerivingI = { derivationSystem :: DerivationSystem SequentLabel DerivationLabel }

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "DerivationsEx1" do
    beforeAll (makeDerivationSystem >>= \derivationSystem -> pure { derivationSystem }) do
      let
        runDerivingTest
          :: forall m a
           . MonadThrow Error m
          => ReaderT (DerivingCtx SortLabel SequentLabel DerivationLabel) (StateT (DerivingEnv SequentLabel DerivationLabel) (ExceptT DerivingError m)) a
          -> DerivingI
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
          makeTest m = runDerivingTest do
            ctx <- ask
            derivation /\ sequent <- m
            Console.log $ "derivation = " <> ctx.derivationSystem.prettyDerivation derivation
            Console.log $ "sequent = " <> ctx.sequentSystem.prettySequent sequent
            pure unit

        it "vz" $ makeTest $ var_ zero_ -- TODO: why doesn't gamma get inferred to be a cons? 
        it "lam" $ makeTest $ lam_ (var_ zero_)
        it "app" $ makeTest $ app_ (var_ (suc_ zero_)) (var_ zero_) -- TODO: why doesn't gamma get inferred to be a double cons?
