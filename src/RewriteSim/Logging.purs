module RewriteSim.Logging where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, catchError)
import Control.Monad.Except (class MonadError, ExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Debug as Debug
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (appendTextFile)
import RewriteSim.Utilities (stringify)

--------------------------------------------------------------------------------

class Monad m <= MonadLogger m where
  log :: LogFn m

log_ :: forall m. MonadLogger m => String -> m Unit
log_ label = log label Nothing

instance MonadLogger Identity where
  log label mbData = Debug.traceM $ showLogMessage label mbData

instance MonadEffect m => MonadLogger (LoggerT m) where
  log label mbData = do
    ctx <- LoggerT ask
    liftEffect $ appendTextFile UTF8 ctx.filepath $ showLogMessage label mbData <> "\n"

showLogMessage :: forall a. String -> Maybe a -> String
showLogMessage label mbData = "[" <> label <> "]" <> maybe "" stringify mbData

--------------------------------------------------------------------------------

newtype LoggerT :: forall k. (k -> Type) -> k -> Type
newtype LoggerT m a = LoggerT (ReaderT LogCtx m a)

unwrapLoggerT :: forall m a. LoggerT m a -> ReaderT LogCtx m a
unwrapLoggerT (LoggerT m) = m

type LogCtx = { filepath :: String }

type LogFn m = forall a. String -> Maybe a -> m Unit

runLoggerT :: forall m a. LoggerT m a -> LogCtx -> m a
runLoggerT (LoggerT m) = runReaderT m

instance Functor m => Functor (LoggerT m) where
  map f (LoggerT m) = LoggerT (map f m)

instance Apply m => Apply (LoggerT m) where
  apply (LoggerT f) (LoggerT a) = LoggerT (apply f a)

instance Applicative m => Applicative (LoggerT m) where
  pure a = LoggerT (pure a)

instance Bind m => Bind (LoggerT m) where
  bind (LoggerT m) k = LoggerT (bind m (k >>> unwrapLoggerT))

instance Monad m => Monad (LoggerT m)

instance MonadTrans LoggerT where
  lift m = LoggerT (lift m)

instance MonadEffect m => MonadEffect (LoggerT m) where
  liftEffect e = LoggerT (liftEffect e)

instance MonadAff m => MonadAff (LoggerT m) where
  liftAff a = LoggerT (liftAff a)

instance MonadThrow e m => MonadThrow e (LoggerT m) where
  throwError e = LoggerT (throwError e)

instance MonadError e m => MonadError e (LoggerT m) where
  catchError (LoggerT m) k = LoggerT (catchError m (k >>> unwrapLoggerT))

liftedLog :: forall t m a. MonadTrans t => Monad m => MonadLogger m => String -> Maybe a -> t m Unit
liftedLog l d = lift (log l d)

instance MonadLogger m => MonadLogger (StateT s m) where
  log = liftedLog

instance MonadLogger m => MonadLogger (ExceptT e m) where
  log = liftedLog

instance MonadLogger m => MonadLogger (ReaderT e m) where
  log = liftedLog

instance (MonadLogger m, Monoid w) => MonadLogger (WriterT w m) where
  log = liftedLog

-- I can add these in later if it turns out I really need them, but I probably
-- don't because the logger effect will almost always be at the top level.

-- instance MonadAsk r m => MonadAsk r (LoggerT m) where
--   ask = LoggerT (lift ask)

-- instance MonadReader r m => MonadReader r (LoggerT m) where
--   local f (LoggerT m) = LoggerT (lift (local f (?a m))) -- (lift (local f m))

-- instance MonadState s m => MonadState s (LoggerT m) where
--   state f = LoggerT (lift (state f))
