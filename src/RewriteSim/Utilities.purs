module RewriteSim.Utilities where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (Error, error)

subReaderT :: forall ctx ctx' m a. MonadReader ctx m => (ctx -> ctx') -> ReaderT ctx' m a -> m a
subReaderT f m = do
  ctx' <- asks f
  m # flip runReaderT ctx'

subStateT :: forall env env' m a. MonadState env m => (env -> env') -> (env' -> env -> env) -> StateT env' m a -> m a
subStateT f g m = do
  env0 <- gets f
  a /\ env1 <- m # flip runStateT env0
  modify_ $ g env1
  pure a

subStateT' :: forall env env' m a. MonadState env m => (env -> env') -> (env' -> env -> env) -> StateT env' m a -> m (a /\ env')
subStateT' f g m = do
  env0 <- gets f
  a /\ env1 <- m # flip runStateT env0
  modify_ $ g env1
  pure $ a /\ env1

ignore :: forall a b. a -> b -> b
ignore _ b = b

runIdentity :: forall a. Identity a -> a
runIdentity = unwrap

apply2M :: forall m a b c. Monad m => (a -> b -> m c) -> m a -> m b -> m c
apply2M f ma mb = do
  a <- ma
  b <- mb
  f a b

applyM :: forall m a b. Monad m => (a -> m b) -> m a -> m b
applyM = bindFlipped

throw :: forall m a. MonadThrow Error m => String -> m a
throw = throwError <<< error

runExceptThrow :: forall m a e. MonadThrow Error m => (e -> String) -> ExceptT e m a -> m a
runExceptThrow f = runExceptT >>> bindFlipped (either (f >>> throw) pure)

foreign import stringify :: forall a. a -> String
