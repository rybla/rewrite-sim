module RewriteSim.Example.Drv where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, gets)
import Data.Array as Array
import Data.Foldable (intercalate, length, traverse_)
import Data.Lens (view, (%~), (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (GenericExpr(..), MetaVar, AbsExpr)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

-- "s" is for "sequent label"

type Sequent :: Type -> Type
type Sequent s = AbsExpr s

type SortSystem sort s =
  { kidSortsOfLabel :: s -> Array sort
  , sortOfLabel :: s -> sort
  , showExpr :: Sequent s -> String
  }

type SortingState sort s =
  { metaVarSorts :: Map MetaVar sort }

type SortingContext sort s =
  { sortSystem :: SortSystem sort s
  , stack :: List (Sequent s)
  }

type SortingError s =
  { stack :: List (Sequent s)
  , message :: String
  }

throwSortingError
  :: forall m sort s a
   . MonadReader (SortingContext sort s) m
  => MonadError (SortingError s) m
  => String
  -> m a
throwSortingError message = do
  ctx <- ask
  throwError
    { stack: ctx.stack
    , message
    }

makeSequent
  :: forall m sort s
   . Show sort
  => Eq sort
  => Show s
  => MonadReader (SortingContext sort s) m
  => MonadState (SortingState sort s) m
  => MonadError (SortingError s) m
  => s
  -> Array (m (Sequent s))
  -> m (Sequent s)
makeSequent s kidsM = do
  ctx <- ask
  kids <- sequence kidsM
  let kidSorts = ctx.sortSystem.kidSortsOfLabel s
  unless (length kidSorts == (length kids :: Int)) do
    throwSortingError $ "A sequent with label " <> show s <> " is expected to have " <> show (length kidSorts :: Int) <> " kids of sorts " <> (kidSorts # map show # intercalate ", " # \s' -> "[" <> s' <> "]") <> " but it actually has " <> show (length kids :: Int) <> " kids."
  Array.zip kidSorts kids # traverse_ case _ of
    expectedKidSort /\ MetaExpr x ->
      gets (view (prop (Proxy @"metaVarSorts") <<< at x)) >>= case _ of
        Nothing -> do
          prop (Proxy @"metaVarSorts") <<< at x .= Just expectedKidSort
        Just actualKidSort -> do
          unless (expectedKidSort == actualKidSort) do
            throwSortingError $ "The sequent meta variable " <> show x <> " is expected to have sort " <> show expectedKidSort <> " but it actually has sort " <> show actualKidSort <> " as inferred from its other appearances."
    expectedKidSort /\ kid@(Expr kidS _) -> do
      let actualKidSort = ctx.sortSystem.sortOfLabel kidS
      unless (expectedKidSort == actualKidSort) do
        throwSortingError $ "The sequent " <> ctx.sortSystem.showExpr kid <> " is expected to have sort " <> show expectedKidSort <> " but it actually has sort " <> show actualKidSort <> "."
  pure $ Expr s kids

-- --------------------------------------------------------------------------------

-- -- "d" is for "derivation label"

-- type Derivation d = AbsExpr d

-- type DerivationRule s =
--   { hypotheses :: Array (Sequent s)
--   , conclusion :: Sequent s
--   }

-- type DerivationSystem s d =
--   { rules :: d -> DerivationRule s
--   }

-- type DerivingState s d =
--   {}

-- type DerivingContext sort s d =
--   { sortSystem :: SortSystem sort s
--   , derivationSystem :: DerivationSystem s d
--   }

-- type DerivingError =
--   { message :: String
--   }

-- throwDerivingError
--   :: forall m sort s d a
--    . MonadReader (DerivingContext sort s d) m
--   => MonadError DerivingError m
--   => String
--   -> m a
-- throwDerivingError message = do
--   throwError
--     { message
--     }

-- makeDerivation
--   :: forall m sort s d
--    . Eq s
--   => Show s
--   => MonadReader (DerivingContext sort s d) m
--   => MonadState (DerivingState s d) m
--   => MonadError DerivingError m
--   => d
--   -> Array (m (Derivation d))
--   -> m (Derivation d)
-- makeDerivation _l kidsM = do
--   kids <- sequence kidsM
--   unsafeCrashWith "TODO"

