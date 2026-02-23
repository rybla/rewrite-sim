module RewriteSim where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, get, modify_)
import Control.Monad.Writer (class MonadWriter, tell)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Foldable (fold, null)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Halogen.HTML (HTML, PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Utilities (ignore, subReaderT, subStateT)

----------------
-- expressions
----------------

data Expr a = Expr a (Array (Expr a))

infix 5 Expr as %

derive instance Generic (Expr a) _

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

renderExpr :: forall m w i a. MonadReader (RenderExprCtx a w i) m => Expr a -> m (HTML w i)
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

----------------
-- rewrite systems
----------------

type Rule a = Expr a -> Maybe (Expr a)

type System a =
  { name :: String
  , rules :: Array (Rule a)
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
  => Expr a
  -> m (Maybe (LocalUpdate a))
simplifyHere e = do
  { system, revPath } <- ask
  let
    go i = case Array.index system.rules i of
      Nothing -> pure Nothing
      Just r -> case r e of
        Nothing -> go (i + 1)
        Just e' -> pure $ Just { path: List.reverse revPath, old: e, new: e' }
  go 0

simplify
  :: forall m ctx env a
   . MonadReader (SimplificationCtx ctx a) m
  => MonadState (SimplificationEnv env a) m
  => Expr a
  -> m (Maybe (LocalUpdate a /\ Expr a))
simplify e0@(Expr a es) = do
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
