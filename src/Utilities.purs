module Utilities where

import Prelude

import Control.Monad.Reader (class MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))

subReaderT :: forall ctx ctx' m a. MonadReader ctx m => (ctx -> ctx') -> ReaderT ctx' m a -> m a
subReaderT f m = do
  ctx' <- asks f
  m # flip runReaderT ctx'

subStateT :: forall env env' m a. MonadState env m => (env -> env') -> (env' -> env -> env) -> StateT env' m a -> m (a /\ env')
subStateT f g m = do
  env0 <- gets f
  a /\ env1 <- m # flip runStateT env0
  modify_ $ g env1
  pure $ a /\ env1

ignore :: forall a b. a -> b -> b
ignore _ b = b

runIdentity :: forall a. Identity a -> a
runIdentity = unwrap
