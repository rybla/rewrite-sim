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
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim (GenericExpr(..), MetaVar)
import RewriteSim.Example.Common (AbsExpr, ExprLabel)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type SequentLabel = ExprLabel
type Sequent = AbsExpr

type SortSystem s =
  { kidSortsOfLabel :: SequentLabel -> Array s
  , sortOfLabel :: SequentLabel -> s
  , showExpr :: Sequent -> String
  }

type SortingState s =
  { metaVarSorts :: Map MetaVar s }

type SortingContext s =
  { sortSystem :: SortSystem s
  , stack :: List Sequent
  }

type SortingError =
  { stack :: List Sequent
  , message :: String
  }

throwSortingError
  :: forall m s a
   . MonadReader (SortingContext s) m
  => MonadError SortingError m
  => String
  -> m a
throwSortingError message = do
  ctx <- ask
  throwError
    { stack: ctx.stack
    , message
    }

makeSequent
  :: forall m s
   . Eq s
  => Show s
  => MonadReader (SortingContext s) m
  => MonadState (SortingState s) m
  => MonadError SortingError m
  => SequentLabel
  -> Array (m Sequent)
  -> m Sequent
makeSequent _l kidsM = do
  _kids <- sequence kidsM
  unsafeCrashWith "TODO"

checkSortOfSequent
  :: forall m s
   . Eq s
  => Show s
  => MonadReader (SortingContext s) m
  => MonadState (SortingState s) m
  => MonadError SortingError m
  => s
  -> Sequent
  -> m Unit
checkSortOfSequent expectedSort sequent =
  local (prop (Proxy @"stack") %~ List.Cons sequent) do
    ctx <- ask
    case sequent of
      MetaExpr x -> do
        gets (view (prop (Proxy @"metaVarSorts") <<< at x)) >>= case _ of
          Nothing -> do
            prop (Proxy @"metaVarSorts") <<< at x .= Just expectedSort
          Just actualSort -> do
            unless (actualSort == expectedSort) do
              throwSortingError $ "The meta variable " <> show sequent <> " is expected to have sort " <> show expectedSort <> " but it actually has sort " <> show actualSort <> " as inferred from its other appearances."
      Expr l kids -> do
        let actualSort = ctx.sortSystem.sortOfLabel l
        unless (actualSort == expectedSort) do
          throwSortingError $ "The sequent " <> ctx.sortSystem.showExpr sequent <> " is expected to have sort " <> show expectedSort <> " but it actually has sort " <> show actualSort <> "."
        let kidSorts = ctx.sortSystem.kidSortsOfLabel l
        unless (length kids == (length kidSorts :: Int)) do
          throwSortingError $ "A sort with label " <> show l <> " is expected to have " <> show (length kidSorts :: Int) <> " kids of sorts " <> (kidSorts # map show # intercalate ", " # \s' -> "[" <> s' <> "]") <> " but it actually has " <> show (length kids :: Int) <> " kids."
        Array.zip kidSorts kids # traverse_ (uncurry checkSortOfSequent)

--------------------------------------------------------------------------------

type DerivationLabel = ExprLabel
type Derivation = AbsExpr

type DerivationRule =
  { label :: DerivationLabel
  , hypotheses :: Array Sequent
  , conclusion :: Sequent
  }

type DerivationSystem =
  { rules :: Array DerivationRule
  }

type DerivationState s =
  {}

type DerivationContext s =
  { sortSystem :: SortSystem s
  }

type DerivationError =
  { message :: String
  }

throwDerivationError
  :: forall m s a
   . MonadReader (DerivationContext s) m
  => MonadError DerivationError m
  => String
  -> m a
throwDerivationError message = do
  throwError
    { message
    }

-- checkSortOfSequent
--   :: forall m s
--    . Eq s
--   => Show s
--   => MonadReader (SortingContext s) m
--   => MonadState (SortingState s) m
--   => MonadError SortingError m
--   => s
--   -> Sequent
--   -> m Unit
-- checkSortOfSequent expectedSort sequent =