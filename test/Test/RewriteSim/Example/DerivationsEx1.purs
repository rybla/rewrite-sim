module Test.RewriteSim.Example.DerivationsEx1 where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import RewriteSim.Example.DerivationsEx1 (makeDerivationSystem, sequentSystem)
import RewriteSim.Example.Library.Derivations (DerivationSystem, DerivingCtx, DerivingEnv, DerivingError, newDerivingCtx, newDerivingEnv)
import RewriteSim.Utilities (runExceptThrow)
import Test.Spec (SpecT, beforeAll, describe, it)

type DerivingI = { derivationSystem :: DerivationSystem String String }

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "DerivationsEx1" do
    beforeAll (makeDerivationSystem >>= \derivationSystem -> pure { derivationSystem }) do
      let
        runDerivingTest
          :: forall m a
           . MonadThrow Error m
          => ReaderT (DerivingCtx String String String) (StateT (DerivingEnv String String) (ExceptT DerivingError m)) a
          -> DerivingI
          -> m a
        runDerivingTest m ctx = do
          m
            # flip runReaderT (newDerivingCtx { sequentSystem, derivationSystem: ctx.derivationSystem })
            # flip evalStateT (newDerivingEnv {})
            # runExceptThrow (\error -> "Deriving error: " <> error.message)
      it "rules" $ runDerivingTest $ pure unit
      describe "derivations" do
        it "lam" $ runDerivingTest $ do
          pure unit
