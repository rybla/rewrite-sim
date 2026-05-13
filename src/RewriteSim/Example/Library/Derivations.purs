module RewriteSim.Example.Library.Derivations where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, gets)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Foldable (intercalate, length, traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object as Object
import RewriteSim (class IsExprLabel, AbsExpr, FresheningT, GenericExpr(..), MetaVar, UnificationEnv, UnificationError(..), freshenAbsExpr, newUnificationEnv, prettyExpr, runFresheningT, substAbsExpr, unify)
import RewriteSim.Logging (class MonadLogger, log, log_)
import RewriteSim.Pretty (class Pretty, pretty)
import RewriteSim.Utilities (mapThrow, stringify, subStateT)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

-- "s" is for "sequent label"

type Sequent :: Type -> Type
type Sequent s = AbsExpr s

type SequentRule sort =
  { hypotheses :: Array sort
  , conclusion :: sort
  }

makeSequentRule :: forall sort. Array sort -> sort -> SequentRule sort
makeSequentRule hypotheses conclusion = { hypotheses, conclusion }

type SequentSystem sort s =
  { rules :: s -> SequentRule sort
  }

type SequentT sort s m = ReaderT (SequentCtx sort s) (StateT (SequentEnv sort s) (ExceptT (SequentError s) m))

type SequentEnv :: Type -> Type -> Type
type SequentEnv sort s =
  { metaSorts :: Map MetaVar sort
  }

newSequentEnv
  :: forall sort s
   . {}
  -> SequentEnv sort s
newSequentEnv {} =
  { metaSorts: Map.empty
  }

type SequentCtx sort s =
  { sequentSystem :: SequentSystem sort s
  }

newSequentCtx
  :: forall sort s
   . { sequentSystem :: SequentSystem sort s }
  -> SequentCtx sort s
newSequentCtx { sequentSystem } =
  { sequentSystem
  }

type SequentError :: Type -> Type
type SequentError s =
  { message :: String
  }

throwSequentError
  :: forall m sort s a
   . MonadReader (SequentCtx sort s) m
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
  => Pretty sort
  => Eq sort
  => IsExprLabel s
  => MonadReader (SequentCtx sort s) m
  => MonadState (SequentEnv sort s) m
  => MonadError (SequentError s) m
  => s
  -> Array (m (Sequent s))
  -> m (Sequent s)
makeSequent s kidsM = do
  ctx <- ask
  kids <- sequence kidsM
  let rule = ctx.sequentSystem.rules s
  unless (length rule.hypotheses == (length kids :: Int)) do
    throwSequentError $ "A sequent with label " <> pretty s <> " is expected to have " <> pretty (length rule.hypotheses :: Int) <> " kids of sorts " <> (rule.hypotheses # map pretty # intercalate ", " # \s' -> "[" <> s' <> "]") <> " but it actually has " <> pretty (length kids :: Int) <> " kids."
  Array.zip rule.hypotheses kids # traverse_ case _ of
    expectedKidSort /\ MetaExpr x ->
      gets (view (prop (Proxy @"metaSorts") <<< at x)) >>= case _ of
        Nothing -> do
          prop (Proxy @"metaSorts") <<< at x .= Just expectedKidSort
        Just actualKidSort -> do
          unless (expectedKidSort == actualKidSort) do
            throwSequentError $ "The sequent metavariable " <> pretty x <> " is expected to have sort " <> pretty expectedKidSort <> " but it actually has sort " <> pretty actualKidSort <> " as inferred from its other appearances."
    expectedKidSort /\ kid@(Expr kidS _) -> do
      let kidRule = ctx.sequentSystem.rules kidS
      unless (expectedKidSort == kidRule.conclusion) do
        throwSequentError $ "The sequent " <> prettyExpr kid <> " is expected to have sort " <> pretty expectedKidSort <> " but it actually has sort " <> pretty kidRule.conclusion <> "."
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
  }

type DerivationRuleT sort s d m = ReaderT (DerivationRuleCtx sort s) (ExceptT (DerivationRuleError d) m)

type DerivationRuleCtx sort s =
  { sequentSystem :: SequentSystem sort s
  }

type DerivationRuleError d =
  { derivationLabel :: d
  , message :: String
  }

makeDerivationRule
  :: forall sort s d m
   . MonadLogger m
  => MonadReader (DerivationRuleCtx sort s) m
  => MonadThrow (DerivationRuleError d) m
  => IsExprLabel d
  => d
  -> Array (SequentT sort s m (Sequent s))
  -> SequentT sort s m (Sequent s)
  -> m (d /\ DerivationRule s)
makeDerivationRule d hypothesesM conclusionM = log "makeDerivationRule" (pure { d: pretty d }) *> do
  ctx <- ask
  let
    runSequentT :: forall a. SequentT sort s m a -> m a
    runSequentT m = m
      # flip runReaderT
          ( newSequentCtx
              { sequentSystem: ctx.sequentSystem
              }
          )
      # flip evalStateT (newSequentEnv {})
      # mapThrow
          ( \error ->
              { derivationLabel: d
              , message: error.message
              }
          )
  hypotheses /\ conclusion <- runSequentT $ Tuple <$> sequence hypothesesM <*> conclusionM
  pure $ d /\ { hypotheses, conclusion }

type DerivingT sort s d m = ReaderT (DerivingCtx sort s d) (StateT (DerivingEnv s d) (ExceptT DerivingError m))

type DerivingEnv :: Type -> Type -> Type
type DerivingEnv s d =
  { metaSub :: Map MetaVar (Sequent s)
  , unificationEnv :: UnificationEnv s
  }

newDerivingEnv
  :: forall s d
   . {}
  -> DerivingEnv s d
newDerivingEnv {} =
  { metaSub: Map.empty
  , unificationEnv: newUnificationEnv {}
  }

type DerivingCtx sort s d =
  { sequentSystem :: SequentSystem sort s
  , derivationSystem :: DerivationSystem s d
  }

newDerivingCtx
  :: forall sort s d
   . { sequentSystem :: SequentSystem sort s
     , derivationSystem :: DerivationSystem s d
     }
  -> DerivingCtx sort s d
newDerivingCtx { sequentSystem, derivationSystem } =
  { sequentSystem
  , derivationSystem
  }

type DerivingError =
  { message :: String
  }

throwDerivingError
  :: forall m sort s d a
   . MonadReader (DerivingCtx sort s d) m
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
   . MonadLogger m
  => MonadReader (DerivingCtx sort s d) m
  => MonadState (DerivingEnv s d) m
  => MonadError DerivingError m
  => IsExprLabel s
  => IsExprLabel d
  => d
  -> Array (m (DerivationAndSequent s d))
  -> m (DerivationAndSequent s d)
makeDerivation d kidsM = do
  log_ ("makeDerivation: " <> stringify d)

  ctx <- ask

  let
    mapThrowUnificationError :: forall a. ExceptT (UnificationError s) m a -> m a
    mapThrowUnificationError =
      mapThrow case _ of
        UnificationError error -> { message: "Failed to unify " <> prettyExpr error.e1 <> " with " <> prettyExpr error.e2 <> " because: " <> error.reason }
        FresheningUnificationError error -> { message: error.message }

    subUnificationState :: forall m' a. MonadState (DerivingEnv s d) m' => StateT (UnificationEnv s) m' a -> m' a
    subUnificationState =
      subStateT
        _.unificationEnv
        (\unificationEnv -> _ { unificationEnv = unificationEnv })

    subFresheningT :: forall a. FresheningT s (StateT (UnificationEnv s) (ExceptT (UnificationError s) m)) a -> m a
    subFresheningT m =
      m
        # runFresheningT
        # subUnificationState
        # mapThrowUnificationError

  let rule = ctx.derivationSystem.rules d
  hypotheses /\ conclusion <- subFresheningT do
    hypotheses <- traverse freshenAbsExpr rule.hypotheses
    conclusion <- freshenAbsExpr rule.conclusion
    pure $ hypotheses /\ conclusion

  log ("makeDerivation: " <> stringify d) $ pure
    { hypotheses: hypotheses # map prettyExpr
    , conclusion: conclusion # prettyExpr
    }

  kids <- sequence kidsM
  Array.zip hypotheses kids
    #
      ( traverse_ case _ of
          _ /\ (MetaExpr x /\ _) -> throwError { message: "A metavariable, " <> pretty x <> ", appeared as a hypothesis of a derivation rule. You _cannot_ use metavariables in place of derivations." }
          expectedKidSequent /\ (kid /\ actualKidSequent) -> do
            unify expectedKidSequent actualKidSequent
              # mapThrow
                  ( case _ of
                      UnificationError error -> { message: "Expected the derivation " <> prettyExpr kid <> " to have a sequent that unified with " <> prettyExpr expectedKidSequent <> ", but failed to unify " <> prettyExpr error.e1 <> " with " <> prettyExpr error.e2 <> " because: " <> error.reason }
                      FresheningUnificationError error -> { message: error.message }
                  )
      )
    -- # flip execStateT (newUnificationEnv {})
    # subUnificationState

  env <- get

  log ("makeDerivation: " <> stringify d) $ pure
    { "unificationEnv.sigma":
        env.unificationEnv.sigma
          # (Map.toUnfoldable :: _ -> List _)
          # map (bimap pretty prettyExpr)
          # Object.fromFoldable
    }

  let conclusionSequent = substAbsExpr env.unificationEnv.sigma conclusion

  log ("makeDerivation: " <> stringify d) $ pure
    { conclusionSequent: conclusionSequent # prettyExpr
    }

  pure $ Expr d (kids # map fst) /\ conclusionSequent
