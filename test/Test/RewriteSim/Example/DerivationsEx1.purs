module Test.RewriteSim.Example.DerivationsEx1 where

import Prelude hiding (zero)

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import RewriteSim (UnificationError(..), freshenAbsExpr, isRenaming, prettyExpr, unify)
import RewriteSim.Example.DerivationsEx1 (DerivationLabel, SequentLabel, SortLabel, app, app_, arr, cons, lam, lam_, mS, makeDerivationSystem, sequentSystem, suc, suc_, typing, var, var_, zero, zero_)
import RewriteSim.Example.Library.Derivations (DerivationAndSequent, DerivationSystem, DerivingT, Sequent, SequentT, newDerivingCtx, newDerivingEnv, runSequentT, subFresheningT, subUnificationState)
import RewriteSim.Logging (LoggerT, log, log_)
import RewriteSim.Pretty (metaquotes, pretty, prettyMap)
import RewriteSim.Utilities (mapThrow, mapThrow', runExceptThrow)
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
    $ do
        it "rules" $ runDerivingTest $ do
          pure unit

        describe "derivations" do
          makeDerivingTest "vz" (var_ zero_)
          makeDerivingTest "λ vz" (lam_ (var_ zero_))
          makeDerivingTest "(vsz) vz" (app_ (var_ (suc_ zero_)) (var_ zero_))

          makeDerivingTestWithExpectedConclusion "vz"
            (var_ zero_)
            ( typing
                (cons (mS "alpha") (mS "gamma"))
                (mS "alpha")
                (var zero)
            )

          makeDerivingTestWithExpectedConclusion "λ vz"
            (lam_ (var_ zero_))
            ( typing
                (mS "gamma")
                (arr (mS "alpha") (mS "alpha"))
                (lam (var zero))
            )

          makeDerivingTestWithExpectedConclusion "(vsz) vz"
            (app_ (var_ (suc_ zero_)) (var_ zero_))
            ( typing
                (cons (mS "alpha") (cons (arr (mS "alpha") (mS "beta")) (mS "gamma")))
                (mS "beta")
                (app (var (suc zero)) (var zero))
            )

          makeDerivingTestWithExpectedConclusion "λ λ (vsz) vz"
            (lam_ (lam_ (app_ (var_ (suc_ zero_)) (var_ zero_))))
            ( typing
                (mS "gamma")
                (arr (arr (mS "alpha") (mS "beta")) (arr (mS "alpha") (mS "beta")))
                (lam (lam (app (var (suc zero)) (var zero))))
            )

          pure unit

        pure unit

setup :: LoggerT Aff DerivationTestInput
setup = do
  derivationSystem <- makeDerivationSystem
  pure { derivationSystem }

runDerivingTest
  :: forall m a
   . MonadThrow Error m
  => DerivingT SortLabel SequentLabel DerivationLabel m a
  -> DerivationTestInput
  -> m a
runDerivingTest m ctx = do
  m
    # flip runReaderT (newDerivingCtx { sequentSystem, derivationSystem: ctx.derivationSystem })
    # flip evalStateT (newDerivingEnv {})
    # runExceptThrow (\error -> "Deriving error: " <> error.message)

makeDerivingTest
  :: String
  -> DerivingT SortLabel SequentLabel DerivationLabel (LoggerT Aff) (DerivationAndSequent SequentLabel DerivationLabel)
  -> LoggerSpecT Aff DerivationTestInput Aff Unit
makeDerivingTest testName m = it testName $ runDerivingTest do
  log_ $ "test: " <> testName
  derivation /\ sequent <- m
  log ("test: " <> testName) $ pure $ "derivation = " <> prettyExpr derivation
  log ("test: " <> testName) $ pure $ "sequent = " <> prettyExpr sequent
  pure unit

makeDerivingTestWithExpectedConclusion
  :: String
  -> DerivingT SortLabel SequentLabel DerivationLabel (LoggerT Aff) (DerivationAndSequent SequentLabel DerivationLabel)
  -> SequentT SortLabel SequentLabel (LoggerT Aff) (Sequent SequentLabel)
  -> LoggerSpecT Aff DerivationTestInput Aff Unit
makeDerivingTestWithExpectedConclusion testName md ms = it testName $ runDerivingTest do
  log_ $ "test: " <> testName

  derivation /\ sequentActual <- md

  sequentExpected_stale <-
    ms
      # runSequentT { sequentSystem }
      # mapThrow' (\error -> { message: "Error when constructing expected sequent: " <> error.message })
      # lift
      # lift
  sequentExpected <- sequentExpected_stale
    # freshenAbsExpr
    # subFresheningT

  -- We reset unification substitution so that after attempting to
  -- unify the actual sequent with expected sequent, we can check
  -- that the substitutions required were only
  -- metavariable-to-metavariable substitutions.
  modify_ \env -> env { unificationEnv { sigma = Map.empty } }

  unify sequentActual sequentExpected
    # mapThrow
        ( case _ of
            UnificationError error -> { message: "When testing the derivation " <> metaquotes (prettyExpr derivation) <> ", expected the actual inferred sequent " <> metaquotes (prettyExpr sequentActual) <> " to unify with the expected sequent " <> metaquotes (prettyExpr sequentExpected) <> ", but failed to unify " <> metaquotes (prettyExpr error.e1) <> " with " <> metaquotes (prettyExpr error.e2) <> " because: " <> error.reason }
            FresheningUnificationError error -> { message: "Expected the actual inferred sequent " <> metaquotes (prettyExpr sequentActual) <> " to unify with the expected sequent " <> metaquotes (prettyExpr sequentExpected) <> ", but encountered freshening error: " <> error.message }
        )
    # subUnificationState

  do
    env <- get
    unless (isRenaming env.unificationEnv.sigma) $
      throwError { message: "Expected the actual sequent " <> metaquotes (prettyExpr sequentActual) <> " to unify with " <> metaquotes (prettyExpr sequentExpected) <> " via a mere metavariable renaming, but these sequents unified with a more complex renaming instead: " <> prettyMap pretty prettyExpr env.unificationEnv.sigma }

  log ("test: " <> testName) $ pure $ "derivation = " <> prettyExpr derivation
  log ("test: " <> testName) $ pure $ "sequentActual = " <> prettyExpr sequentActual
  log ("test: " <> testName) $ pure $ "sequentExpected = " <> prettyExpr sequentExpected

  pure unit

