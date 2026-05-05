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
   . Eq s
  => Show s
  => MonadReader (SortingContext sort s) m
  => MonadState (SortingState sort s) m
  => MonadError (SortingError s) m
  => s
  -> Array (m (Sequent s))
  -> m (Sequent s)
makeSequent _l kidsM = do
  _kids <- sequence kidsM
  unsafeCrashWith "TODO"

checkSortOfSequent
  :: forall m sort s
   . Show sort
  => Eq sort
  => Show s
  => Eq s
  => MonadReader (SortingContext sort s) m
  => MonadState (SortingState sort s) m
  => MonadError (SortingError s) m
  => sort
  -> Sequent s
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

