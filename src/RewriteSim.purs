module RewriteSim where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, execStateT, get, gets, modify_)
import Control.Monad.Writer (class MonadWriter, tell)
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (fold, foldl, length, null, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (%=), (.=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Halogen.HTML (HTML, PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import RewriteSim.Logging (class MonadLogger)
import RewriteSim.Pretty (class Pretty, pretty)
import RewriteSim.Utilities (ignore, subReaderT, subStateT)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- metavariables
--------------------------------------------------------------------------------

newtype MetaVar = MetaVar { label :: String, index :: Int }

derive instance Generic MetaVar _

instance Show MetaVar where
  show x = genericShow x

instance Pretty MetaVar where
  pretty (MetaVar v) = v.label <> if v.index == -1 then "" else "@" <> show v.index

instance Eq MetaVar where
  eq x = genericEq x

instance Ord MetaVar where
  compare x = genericCompare x

--------------------------------------------------------------------------------
-- expressions
--------------------------------------------------------------------------------

-- | A concrete expression, which *cannot* contain metavariables.
type Expr = GenericExpr Void

asExpr :: forall a. Expr a -> a /\ Array (Expr a)
asExpr (MetaExpr v) = absurd v
asExpr (Expr a es) = a /\ es

-- | An abstract expression, which *can* contain metavariables.
type AbsExpr = GenericExpr MetaVar

collectMetas :: forall x a. Ord x => GenericExpr x a -> Set x
collectMetas = collectMetas' Set.empty

collectMetas' :: forall x a. Ord x => Set x -> GenericExpr x a -> Set x
collectMetas' vs (MetaExpr v) = Set.insert v vs
collectMetas' vs (Expr _ es) = foldl collectMetas' vs es

data GenericExpr x a
  = MetaExpr x
  | Expr a (Array (GenericExpr x a))

infix 5 Expr as %

derive instance Generic (GenericExpr x a) _

derive instance Bifunctor GenericExpr

derive instance Functor (GenericExpr x)

instance (Eq x, Eq a) => Eq (GenericExpr x a) where
  eq x = genericEq x

instance (Show x, Show a) => Show (GenericExpr x a) where
  show x = genericShow x

instance (Pretty x, Pretty a) => Pretty (GenericExpr x a) where
  pretty (Expr a es) = "(" <> pretty a <> " % " <> pretty es <> ")"
  pretty (MetaExpr x) = pretty x

me :: forall a. String -> AbsExpr a
me label = MetaExpr (mv label)

mv :: String -> MetaVar
mv label = MetaVar { label, index: -1 }

type RenderExprCtx x a w i =
  { renderX :: x -> HTML w i
  , renderA :: a -> HTML w i
  , mb_highlightPath :: Maybe Path
  }

newRenderExprCtx
  :: forall x a w i
   . { renderX :: x -> HTML w i
     , renderA :: a -> HTML w i
     , mb_highlightPath :: Maybe Path
     }
  -> RenderExprCtx x a w i
newRenderExprCtx { renderX, renderA, mb_highlightPath } =
  { renderX
  , renderA
  , mb_highlightPath
  }

renderExpr
  :: forall m w i x a
   . MonadReader (RenderExprCtx x a w i) m
  => GenericExpr x a
  -> m (HTML w i)
renderExpr (MetaExpr x) = do
  { renderX, mb_highlightPath } <- ask
  let r_x = x # renderX
  pure $
    HH.div
      [ HP.classes $ fold
          [ [ HH.ClassName "expr"
            , HH.ClassName "leaf"
            , HH.ClassName "meta"
            ]
          , case mb_highlightPath of
              Just List.Nil -> [ HH.ClassName "highlight" ]
              _ -> []
          ]
      ]
      [ HH.div [ HP.classes [ HH.ClassName "expr-label" ] ] [ r_x ]
      ]
renderExpr (Expr a es) = do
  { renderA, mb_highlightPath } <- ask
  let r_a = a # renderA
  r_es <- es # traverseWithIndex \i e ->
    local
      ( \ctx -> ctx
          { mb_highlightPath = do
              { head: i', tail: highlightPath' } <- List.uncons =<< ctx.mb_highlightPath
              guard $ i == i'
              pure highlightPath'
          }
      )
      do
        renderExpr e
  pure $
    HH.div
      [ HP.classes $ fold
          [ [ HH.ClassName "expr"
            , HH.ClassName if null es then "leaf" else "branch"
            ]
          , case mb_highlightPath of
              Just List.Nil -> [ HH.ClassName "highlight" ]
              _ -> []
          ]
      ]
      [ HH.div [ HP.classes [ HH.ClassName "expr-label" ] ] [ r_a ]
      , HH.div [ HP.classes [ HH.ClassName "expr-kids" ] ] r_es
      ]

--------------------------------------------------------------------------------
-- unification
--------------------------------------------------------------------------------

type AbsExprSubst a = Map MetaVar (AbsExpr a)

substAbsExprToExpr :: forall a. AbsExprSubst a -> AbsExpr a -> Expr a
substAbsExprToExpr sigma (MetaExpr x) = case Map.lookup x sigma of
  Nothing -> unsafeCrashWith $ "Unknown metavariable: " <> show x
  Just e -> substAbsExprToExpr sigma e
substAbsExprToExpr sigma (Expr a es) = Expr a (map (substAbsExprToExpr sigma) es)

substAbsExpr :: forall a. AbsExprSubst a -> AbsExpr a -> AbsExpr a
substAbsExpr sigma e@(MetaExpr x) = case Map.lookup x sigma of
  Nothing -> e
  Just e' -> substAbsExpr sigma e'
substAbsExpr sigma (Expr a es) = Expr a (map (substAbsExpr sigma) es)

type UnificationEnv a =
  { freshIndex :: Int
  , sigma :: AbsExprSubst a
  }

newUnificationEnv :: forall a. {} -> UnificationEnv a
newUnificationEnv {} =
  { freshIndex: 1
  , sigma: Map.empty
  }

type UnificationError a =
  { e1 :: AbsExpr a
  , e2 :: AbsExpr a
  , reason :: String
  }

unifyMeta
  :: forall m a
   . MonadLogger m
  => MonadThrow (UnificationError a) m
  => MonadState (UnificationEnv a) m
  => Eq a
  => MetaVar
  -> AbsExpr a
  -> m Unit
unifyMeta x e = do
  when (Set.member x (collectMetas e)) $ throwError { e1: MetaExpr x, e2: e, reason: "infinite assignment" }
  gets (view (prop (Proxy @"sigma") <<< at x)) >>= case _ of
    Nothing -> prop (Proxy @"sigma") <<< at x .= Just e
    Just e' -> unify e e'

unify
  :: forall m a
   . MonadLogger m
  => MonadThrow (UnificationError a) m
  => MonadState (UnificationEnv a) m
  => Eq a
  => AbsExpr a
  -> AbsExpr a
  -> m Unit
-- unify (MetaExpr x) e = unifyMeta x e
-- unify e (MetaExpr x) = unifyMeta x e
-- unify e1@(Expr a1 es1) e2@(Expr a2 es2) = do
unify e1 e2 = case e1 /\ e2 of
  MetaExpr x /\ e -> unifyMeta x e
  e /\ MetaExpr x -> unifyMeta x e
  Expr a1 es1 /\ Expr a2 es2 -> do
    unless (a1 == a2) do throwError { e1, e2, reason: "different heads" }
    unless (eq @Int (length es1) (length es2)) do throwError { e1, e2, reason: "different arities" }
    Array.zip es1 es2 # traverse_ (uncurry unify)

freshIndex :: forall m a. MonadState (UnificationEnv a) m => m Int
freshIndex = do
  i <- gets (view (prop (Proxy @"freshIndex")))
  prop (Proxy @"freshIndex") %= (1 + _)
  pure i

freshenMetaVar :: forall m a. MonadState (UnificationEnv a) m => MetaVar -> m MetaVar
freshenMetaVar (MetaVar v) = do
  i <- freshIndex
  pure $ MetaVar v { index = i }

freshenAbsExpr :: forall m a. MonadState (UnificationEnv a) m => AbsExpr a -> m (AbsExpr a)
freshenAbsExpr (MetaExpr x) = MetaExpr <$> freshenMetaVar x
freshenAbsExpr (Expr a es) = Expr a <$> traverse freshenAbsExpr es

freshenRule :: forall m a. MonadState (UnificationEnv a) m => Rule a -> m (Rule a)
freshenRule (Rule rule) = do
  input' <- freshenAbsExpr rule.input
  output' <- freshenAbsExpr rule.output
  pure $ Rule rule
    { input = input'
    , output = output'
    }

--------------------------------------------------------------------------------
-- rewrite systems
--------------------------------------------------------------------------------

newtype Rule a = Rule
  { name :: String
  , input :: AbsExpr a
  , output :: AbsExpr a
  }

newRule :: forall a. String -> AbsExpr a -> AbsExpr a -> Rule a
newRule name input output = Rule { name, input, output }

applyRule
  :: forall m a
   . MonadLogger m
  => MonadState (UnificationEnv a) m
  => Eq a
  => Rule a
  -> Expr a
  -> m (Maybe (Expr a))
applyRule (Rule r) e = do
  input <- freshenAbsExpr r.input
  output <- freshenAbsExpr r.output
  err_or_env <- unify input (bimap absurd identity e)
    # flip execStateT (newUnificationEnv {})
    # runExceptT
  case err_or_env of
    Left _err -> pure Nothing
    Right env -> pure $ Just $ substAbsExprToExpr env.sigma output

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
  { unificationEnv :: UnificationEnv a
  | env
  }

newSimplificationEnv :: forall env a. {} -> Record env -> SimplificationEnv env a
newSimplificationEnv {} = Record.union
  { unificationEnv: newUnificationEnv {}
  }

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
   . MonadLogger m
  => MonadReader (SimplificationCtx ctx a) m
  => MonadState (SimplificationEnv env a) m
  => Eq a
  => Expr a
  -> m (Maybe (LocalUpdate a))
simplifyHere e = do
  { system, revPath } <- ask
  let
    go i = case Array.index system.rules i of
      Nothing -> pure Nothing
      Just r -> applyRule r e # subStateT _.unificationEnv (\unificationEnv -> _ { unificationEnv = unificationEnv }) >>= case _ of
        Nothing -> go (i + 1)
        Just e' -> pure $ Just { path: List.reverse revPath, old: e, new: e' }
  go 0

simplify
  :: forall m ctx env a
   . MonadLogger m
  => MonadReader (SimplificationCtx ctx a) m
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

--------------------------------------------------------------------------------
-- normalization
--------------------------------------------------------------------------------

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
   . MonadLogger m
  => MonadReader (NormalizationCtx ctx a) m
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
  mb_e' <- simplify e
    # subReaderT (\ctx -> newSimplificationCtx { system: ctx.system } ctx)
    # subStateT (newSimplificationEnv {}) ignore
  case mb_e' of
    Nothing -> pure $ e
    Just (update /\ e') -> do
      tell [ { update, old: e, new: e' } ]
      normalize e'
