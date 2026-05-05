module RewriteSim.Example.Library.Derivations where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, execStateT, gets)
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (intercalate, length, traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import RewriteSim (AbsExpr, GenericExpr(..), MetaVar, newUnificationEnv, substAbsExpr, unify)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

mapThrow :: forall e1 e2 m a. MonadThrow e2 m => (e1 -> e2) -> ExceptT e1 m a -> m a
mapThrow f m = m # runExceptT >>= either (f >>> throwError) pure

--------------------------------------------------------------------------------

-- "s" is for "sequent label"

type Sequent :: Type -> Type
type Sequent s = AbsExpr s

type SequentRule sort =
  { hypotheses :: Array sort
  , conclusion :: sort
  }

type SequentSystem sort s =
  { rules :: s -> SequentRule sort
  , showSequent :: Sequent s -> String
  }

type SequentM sort s m = ReaderT (SequentContext sort s) (StateT (SequentState sort s) (ExceptT (SequentError s) m))

type SequentState :: Type -> Type -> Type
type SequentState sort s =
  { metaSorts :: Map MetaVar sort
  }

newSequentState
  :: forall sort s
   . {}
  -> SequentState sort s
newSequentState {} =
  { metaSorts: Map.empty
  }

type SequentContext sort s =
  { sequentSystem :: SequentSystem sort s
  }

newSequentContext
  :: forall sort s
   . { sequentSystem :: SequentSystem sort s }
  -> SequentContext sort s
newSequentContext { sequentSystem } =
  { sequentSystem
  }

type SequentError :: Type -> Type
type SequentError s =
  { message :: String
  }

throwSequentError
  :: forall m sort s a
   . MonadReader (SequentContext sort s) m
  => MonadError (SequentError s) m
  => String
  -> m a
throwSequentError message = do
  throwError
    { message
    }

infix 1 makeSequent as %%

makeSequent
  :: forall m sort s
   . Show sort
  => Eq sort
  => Show s
  => MonadReader (SequentContext sort s) m
  => MonadState (SequentState sort s) m
  => MonadError (SequentError s) m
  => s
  -> Array (m (Sequent s))
  -> m (Sequent s)
makeSequent s kidsM = do
  ctx <- ask
  kids <- sequence kidsM
  let rule = ctx.sequentSystem.rules s
  unless (length rule.hypotheses == (length kids :: Int)) do
    throwSequentError $ "A sequent with label " <> show s <> " is expected to have " <> show (length rule.hypotheses :: Int) <> " kids of sorts " <> (rule.hypotheses # map show # intercalate ", " # \s' -> "[" <> s' <> "]") <> " but it actually has " <> show (length kids :: Int) <> " kids."
  Array.zip rule.hypotheses kids # traverse_ case _ of
    expectedKidSort /\ MetaExpr x ->
      gets (view (prop (Proxy @"metaSorts") <<< at x)) >>= case _ of
        Nothing -> do
          prop (Proxy @"metaSorts") <<< at x .= Just expectedKidSort
        Just actualKidSort -> do
          unless (expectedKidSort == actualKidSort) do
            throwSequentError $ "The sequent metavariable " <> show x <> " is expected to have sort " <> show expectedKidSort <> " but it actually has sort " <> show actualKidSort <> " as inferred from its other appearances."
    expectedKidSort /\ kid@(Expr kidS _) -> do
      let kidRule = ctx.sequentSystem.rules kidS
      unless (expectedKidSort == kidRule.conclusion) do
        throwSequentError $ "The sequent " <> ctx.sequentSystem.showSequent kid <> " is expected to have sort " <> show expectedKidSort <> " but it actually has sort " <> show kidRule.conclusion <> "."
  pure $ Expr s kids

--------------------------------------------------------------------------------

-- "d" is for "derivation label"

type Derivation d = AbsExpr d

type DerivationAndSequent s d = Derivation d /\ Sequent s

type DerivationRule s =
  { hypotheses :: Array (Sequent s)
  , conclusion :: Sequent s
  }

type DerivationSystem s d =
  { rules :: d -> DerivationRule s
  , showDerivation :: Derivation d -> String
  }

type DerivationRuleContext sort s =
  { sequentSystem :: SequentSystem sort s
  }

type DerivationRuleError d =
  { derivationLabel :: d
  , message :: String
  }

makeDerivationRule
  :: forall sort s d m
   . MonadReader (DerivationRuleContext sort s) m
  => MonadThrow (DerivationRuleError d) m
  => d
  -> Array (SequentM sort s m (Sequent s))
  -> SequentM sort s m (Sequent s)
  -> m (DerivationRule s)
makeDerivationRule d hypothesesM conclusionM = do
  ctx <- ask
  let
    runSequentM :: forall a. SequentM sort s m a -> m a
    runSequentM m = m
      # flip runReaderT
          ( newSequentContext
              { sequentSystem: ctx.sequentSystem
              }
          )
      # flip evalStateT (newSequentState {})
      # mapThrow
          ( \error ->
              { derivationLabel: d
              , message: error.message
              }
          )
  hypotheses /\ conclusion <- runSequentM $ Tuple <$> sequence hypothesesM <*> conclusionM
  pure { hypotheses, conclusion }

type DerivingState :: Type -> Type -> Type
type DerivingState s d =
  { metaSub :: Map MetaVar (Sequent s)
  }

type DerivingContext sort s d =
  { sequentSystem :: SequentSystem sort s
  , derivationSystem :: DerivationSystem s d
  }

type DerivingError =
  { message :: String
  }

throwDerivingError
  :: forall m sort s d a
   . MonadReader (DerivingContext sort s d) m
  => MonadError DerivingError m
  => String
  -> m a
throwDerivingError message = do
  throwError
    { message
    }

infix 1 makeDerivation as %

makeDerivation
  :: forall m sort s d
   . Eq s
  => MonadReader (DerivingContext sort s d) m
  => MonadState (DerivingState s d) m
  => MonadError DerivingError m
  => d
  -> Array (m (DerivationAndSequent s d))
  -> m (DerivationAndSequent s d)
makeDerivation d kidsM = do
  ctx <- ask
  let rule = ctx.derivationSystem.rules d
  kids <- sequence kidsM
  unificationEnv <- Array.zip rule.hypotheses kids
    #
      ( traverse_ case _ of
          _ /\ (MetaExpr x /\ _) -> throwError { message: "A metavariable, " <> show x <> ", appeared as a hypothesis of a derivation rule. You _cannot_ use metavariables in place of derivations." }
          expectedKidSequent /\ (kid /\ actualKidSequent) -> do
            unify expectedKidSequent actualKidSequent
              # mapThrow (\error -> { message: "Expected the derivation " <> ctx.derivationSystem.showDerivation kid <> " to have a sequent that unified with " <> ctx.sequentSystem.showSequent expectedKidSequent <> ", but failed to unify " <> ctx.sequentSystem.showSequent error.e1 <> " with " <> ctx.sequentSystem.showSequent error.e2 <> " because " <> error.reason })
      )
    # flip execStateT (newUnificationEnv {})
  let conclusionSequent = substAbsExpr unificationEnv.sigma rule.conclusion
  pure $ Expr d (kids # map fst) /\ conclusionSequent
