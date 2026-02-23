module RewriteSim where

import Prelude

import Control.Monad.Reader (class MonadReader, ask)
import Data.Array as Array
import Data.Eq.Generic (genericEq)
import Data.Foldable (null)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)

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

renderExpr :: forall w i a. (a -> HTML w i) -> Expr a -> HTML w i
renderExpr renderA (Expr a es) =
  HH.div [ HP.classes [ HH.ClassName "expr", HH.ClassName if null es then "leaf" else "branch" ] ]
    [ HH.div [ HP.classes [ HH.ClassName "expr-label" ] ] [ renderA a ]
    , HH.div [ HP.classes [ HH.ClassName "expr-kids" ] ] $ map (renderExpr renderA) $ es
    ]

----------------
-- rewrite systems
----------------

type Rule a = Expr a -> Maybe (Expr a)

type System a =
  { name :: String
  , rules :: Array (Rule a)
  }

----------------
-- rewrite engine
----------------

type EngineCtx a r =
  { system :: System a
  | r
  }

stepHere :: forall m r a. MonadReader (EngineCtx a r) m => Expr a -> m (Maybe (Expr a))
stepHere e = do
  { system } <- ask
  let
    go i = case Array.index system.rules i of
      Nothing -> pure Nothing
      Just r -> case r e of
        Nothing -> go (i + 1)
        Just e' -> pure $ Just e'
  go 0

step :: forall m r a. MonadReader (EngineCtx a r) m => Expr a -> m (Maybe (Expr a))
step e0@(Expr a es) = do
  let
    go i = case Array.index es i of
      Nothing -> stepHere e0
      Just e -> step e >>= case _ of
        Nothing -> go (i + 1)
        Just e' -> do
          let es' = es # Array.updateAt i e' # fromMaybe' \_ -> unsafeCrashWith "impossible"
          pure $ Just $ Expr a es'
  go 0

