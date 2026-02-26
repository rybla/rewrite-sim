module RewriteSim where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, get, gets, modify_, runStateT)
import Control.Monad.Writer (class MonadWriter, tell)
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Foldable (fold, foldM, length, null, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Halogen.HTML (HTML, PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import RewriteSim.Utilities (ignore, subReaderT, subStateT)
import Type.Proxy (Proxy(..))

----------------
-- metavariables
----------------

newtype MetaVar = MetaVar { label :: String, index :: Int }

derive instance Generic MetaVar _

instance Show MetaVar where
  show x = genericShow x

instance Eq MetaVar where
  eq x = genericEq x

instance Ord MetaVar where
  compare x = genericCompare x

----------------
-- expressions
----------------

-- | A concrete expression, which CANNOT contain metavariables.
type Expr = GenericExpr Void

asExpr :: forall a. Expr a -> a /\ Array (Expr a)
asExpr (MetaExpr v) = absurd v
asExpr (Expr a es) = a /\ es

-- | An abstract expression, which CAN contain metavariables.
type AbsExpr = GenericExpr MetaVar

data GenericExpr x a = MetaExpr x | Expr a (Array (GenericExpr x a))

infix 5 Expr as %

derive instance Generic (GenericExpr x a) _

derive instance Bifunctor GenericExpr

derive instance Functor Expr

instance Eq a => Eq (Expr a) where
  eq x = genericEq x

instance Show a => Show (Expr a) where
  show x = genericShow x

type RenderExprCtx a w i =
  { renderA :: a -> HTML w i
  , mb_highlightPath :: Maybe Path
  }

newRenderExprCtx
  :: forall a w i
   . { renderA :: a -> HTML w i
     , mb_highlightPath :: Maybe Path
     }
  -> RenderExprCtx a w i
newRenderExprCtx { renderA, mb_highlightPath } =
  { renderA
  , mb_highlightPath
  }

-- renderExpr :: forall m w i a. MonadReader (RenderExprCtx a w i) m => Expr a -> m (HTML w i)
-- renderExpr (Expr a es) = do
--   { renderA, mb_highlightPath } <- ask
--   let r_a = a # renderA
--   r_es <- es # traverseWithIndex \i e ->
--     local
--       ( \ctx -> ctx
--           { mb_highlightPath = do
--               { head: i', tail: highlightPath' } <- List.uncons =<< ctx.mb_highlightPath
--               guard $ i == i'
--               pure highlightPath'
--           }
--       )
--       do
--         renderExpr e
--   pure $
--     HH.div
--       [ HP.classes $ fold
--           [ [ HH.ClassName "expr"
--             , HH.ClassName if null es then "leaf" else "branch"
--             ]
--           , case mb_highlightPath of
--               Just List.Nil -> [ HH.ClassName "highlight" ]
--               _ -> []
--           ]
--       ]
--       [ HH.div [ HP.classes [ HH.ClassName "expr-label" ] ] [ r_a ]
--       , HH.div [ HP.classes [ HH.ClassName "expr-kids" ] ] r_es
--       ]

----------------
-- unification
----------------

type AbsExprSubst a = Map MetaVar (AbsExpr a)

type UnificationEnv a =
  { freshCounter :: Int
  , sigma :: AbsExprSubst a
  }

newUnificationEnv :: forall a. {} -> UnificationEnv a
newUnificationEnv {} =
  { freshCounter: 0
  , sigma: Map.empty
  }

type UnificationError a =
  { e1 :: AbsExpr a
  , e2 :: AbsExpr a
  , reason :: String
  }

unifyMeta
  :: forall m a
   . MonadThrow (UnificationError a) m
  => MonadState (UnificationEnv a) m
  => Eq a
  => MetaVar
  -> AbsExpr a
  -> m Unit
unifyMeta x e = do
  gets (view (prop (Proxy @"sigma") <<< at x)) >>= case _ of
    Nothing -> prop (Proxy @"sigma") <<< at x .= Just e
    Just e' -> unify e e'

unify
  :: forall m a
   . MonadThrow (UnificationError a) m
  => MonadState (UnificationEnv a) m
  => Eq a
  => AbsExpr a
  -> AbsExpr a
  -> m Unit
unify (MetaExpr x) e = unifyMeta x e
unify e (MetaExpr x) = unifyMeta x e
unify e1@(Expr a1 es1) e2@(Expr a2 es2) = do
  unless (a1 == a2) do throwError { e1, e2, reason: "different heads" }
  unless (eq @Int (length es1) (length es2)) do throwError { e1, e2, reason: "different arities" }
  Array.zip es1 es2 # traverse_ (uncurry unify)

----------------
-- rewrite systems
----------------

newtype Rule a = Rule
  { name :: String
  , input :: AbsExpr a
  , output :: AbsExpr a
  }

applyRule :: forall m a. Monad m => Eq a => Rule a -> Expr a -> m (Maybe (Expr a))
applyRule (Rule r) e = do
  _ <- unify r.input (bimap absurd identity e)
    # flip runStateT (newUnificationEnv {})
    # runExceptT
  pure Nothing

mapRule :: forall a b. (a -> b) -> Rule a -> Rule b
mapRule f (Rule r) = Rule
  { name: r.name
  , input: rmap f r.input
  , output: rmap f r.output
  }

type System a =
  { name :: String
  , rules :: Array (Rule a)
  }

mapSystem :: forall a b. (a -> b) -> System a -> System b
mapSystem f1 system =
  { name: system.name
  , rules: map (mapRule f1) system.rules
  }

type Path = List Int

----------------
-- simplification
----------------

type SimplificationCtx ctx a =
  { system :: System a
  , revPath :: Path
  | ctx
  }

newSimplificationCtx :: forall ctx a. { system :: System a } -> Record ctx -> SimplificationCtx ctx a
newSimplificationCtx { system } = Record.union
  { system
  , revPath: none
  }

type SimplificationEnv :: Row Type -> Type -> Type
type SimplificationEnv env a =
  {
  | env
  }

newSimplificationEnv :: forall env a. {} -> Record env -> SimplificationEnv env a
newSimplificationEnv {} = Record.union
  {}

type LocalUpdate a =
  { path :: Path
  , old :: Expr a
  , new :: Expr a
  }

type GlobalUpdate a =
  { update :: LocalUpdate a
  , old :: Expr a
  , new :: Expr a
  }

simplifyHere
  :: forall m ctx env a
   . MonadReader (SimplificationCtx ctx a) m
  => MonadState (SimplificationEnv env a) m
  => Eq a
  => Expr a
  -> m (Maybe (LocalUpdate a))
simplifyHere e = do
  { system, revPath } <- ask
  let
    go i = case Array.index system.rules i of
      Nothing -> pure Nothing
      Just r -> applyRule r e >>= case _ of
        Nothing -> go (i + 1)
        Just e' -> pure $ Just { path: List.reverse revPath, old: e, new: e' }
  go 0

simplify
  :: forall m ctx env a
   . MonadReader (SimplificationCtx ctx a) m
  => MonadState (SimplificationEnv env a) m
  => Eq a
  => Expr a
  -> m (Maybe (LocalUpdate a /\ Expr a))
simplify e0 = e0 # asExpr # \(a /\ es) -> do
  let
    go i = case Array.index es i of
      Nothing -> simplifyHere e0 >>= case _ of
        Nothing -> pure Nothing
        Just update -> pure $ Just $ update /\ update.new
      Just e -> simplify e >>= case _ of
        Nothing -> go (i + 1)
        Just (update /\ e') -> do
          let es' = es # Array.updateAt i e' # fromMaybe' \_ -> unsafeCrashWith "impossible"
          pure $ Just $ update /\ Expr a es'
  go 0

----------------
-- normalization
----------------

type NormalizationCtx ctx a =
  { system :: System a
  | ctx
  }

newNormalizationCtx :: forall ctx a. { system :: System a } -> Record ctx -> NormalizationCtx ctx a
newNormalizationCtx { system } = Record.union
  { system
  }

type NormalizationEnv :: Row Type -> Type -> Type
type NormalizationEnv env a =
  { gas :: Int
  | env
  }

newNormalizationEnv
  :: forall env a
   . { gas :: Int
     }
  -> Record env
  -> NormalizationEnv env a
newNormalizationEnv { gas } = Record.union
  { gas }

type NormalizationTrace a = Array (GlobalUpdate a)

normalize
  :: forall m ctx env a
   . MonadReader (NormalizationCtx ctx a) m
  => MonadState (NormalizationEnv env a) m
  => MonadWriter (NormalizationTrace a) m
  => MonadThrow PlainHTML m
  => Eq a
  => Expr a
  -> m (Expr a)
normalize e = do
  { gas } <- get
  if 0 < gas then
    modify_ \state -> state { gas = state.gas - 1 }
  else
    throwError $ HH.text "out of gas"
  mb_e' /\ _env' <- simplify e
    # subReaderT (\ctx -> newSimplificationCtx { system: ctx.system } ctx)
    # subStateT (newSimplificationEnv {}) ignore
  case mb_e' of
    Nothing -> pure $ e
    Just (update /\ e') -> do
      tell [ { update, old: e, new: e' } ]
      normalize e'
