module RewriteSim.Logging where

import Prelude

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Maybe (Maybe, maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (appendTextFile)
import RewriteSim.Utilities (stringify)

--------------------------------------------------------------------------------

class MonadLogger m where
  log :: LogFn m

instance MonadEffect m => MonadLogger (LoggerT m) where
  log label mbData = do
    ctx <- LoggerT ask
    liftEffect $ appendTextFile UTF8 ctx.filepath $ "[" <> label <> "]" <> maybe "" stringify mbData

--------------------------------------------------------------------------------

newtype LoggerT m a = LoggerT (ReaderT (LogCtx m) m a)

type LogCtx m = { filepath :: String, log :: LogFn m }

type LogFn m = forall a. String -> Maybe a -> m Unit

instance Functor m => Functor (LoggerT m) where
  map f (LoggerT m) = LoggerT (map f m)

instance Apply m => Apply (LoggerT m) where
  apply (LoggerT f) (LoggerT a) = LoggerT (apply f a)

instance Applicative m => Applicative (LoggerT m) where
  pure a = LoggerT (pure a)

instance Bind m => Bind (LoggerT m) where
  bind (LoggerT m) k = LoggerT (bind m (k >>> \(LoggerT m') -> m'))

instance Monad m => Monad (LoggerT m)

instance MonadTrans LoggerT where
  lift m = LoggerT (lift m)

instance MonadEffect m => MonadEffect (LoggerT m) where
  liftEffect e = LoggerT (liftEffect e)

-- I can add these in later if it turns out I really need them, but I probably
-- don't because the logger effect will almost always be at the top level.

-- instance MonadAsk r m => MonadAsk r (LoggerT m) where
--   ask = LoggerT (lift ask)

-- instance MonadReader r m => MonadReader r (LoggerT m) where
--   local f (LoggerT m) = LoggerT (lift (local f (?a m))) -- (lift (local f m))
