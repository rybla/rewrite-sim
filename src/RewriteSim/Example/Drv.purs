module RewriteSim.Example.Drv where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, gets, modify_)
import Data.Array as Array
import Data.Foldable (intercalate, length, traverse_)
import Data.Lens (view, (%~), (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import RewriteSim (GenericExpr(..), MetaVar(..), asExpr)
import RewriteSim.Example.Common (Expr, ExprLabel, AbsExpr)
import Type.Proxy (Proxy(..))

type SortSystem s =
  { kidSortsOfLabel :: ExprLabel -> Array s
  , sortOfLabel :: ExprLabel -> s
  , showExpr :: AbsExpr -> String
  }

type SortingState s =
  { metaVarSorts :: Map MetaVar s }

type SortingContext s =
  { sortSystem :: SortSystem s
  , stack :: List AbsExpr
  }

type SortingError =
  { stack :: List AbsExpr
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

checkSort
  :: forall m s
   . Eq s
  => Show s
  => MonadReader (SortingContext s) m
  => MonadState (SortingState s) m
  => MonadError SortingError m
  => s
  -> AbsExpr
  -> m Unit
checkSort expectedSort e =
  local (prop (Proxy @"stack") %~ List.Cons e) do
    ctx <- ask
    case e of
      MetaExpr x -> do
        gets (view (prop (Proxy @"metaVarSorts") <<< at x)) >>= case _ of
          Nothing -> do
            prop (Proxy @"metaVarSorts") <<< at x .= Just expectedSort
          Just actualSort -> do
            unless (actualSort == expectedSort) do
              throwSortingError $ "The meta variable " <> show e <> " is expected to have sort " <> show expectedSort <> " but it actually has sort " <> show actualSort <> " as inferred from its other appearances."
      Expr l kids -> do
        let actualSort = ctx.sortSystem.sortOfLabel l
        unless (actualSort == expectedSort) do
          throwSortingError $ "The expression " <> ctx.sortSystem.showExpr e <> " is expected to have sort " <> show expectedSort <> " but it actually has sort " <> show actualSort <> "."
        let kidSorts = ctx.sortSystem.kidSortsOfLabel l
        unless (length kids == (length kidSorts :: Int)) do
          throwSortingError $ "A sort with label " <> show l <> " is expected to have " <> show (length kidSorts :: Int) <> " kids of sorts " <> (kidSorts # map show # intercalate ", " # \s' -> "[" <> s' <> "]") <> " but it actually has " <> show (length kids :: Int) <> " kids."
        Array.zip kidSorts kids # traverse_ (uncurry checkSort)

type Rule =
  { label :: String
  , hypotheses :: Array AbsExpr
  , conclusion :: AbsExpr
  }

type RuleSystem = {}

-- type Rule = {

-- }

-- checkDerivation :: Expr -> 

-- checkExpr ::  Expr -> m Expr
