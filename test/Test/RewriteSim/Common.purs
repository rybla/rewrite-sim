module Test.RewriteSim.Common where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Node.FS.Sync as FS
import RewriteSim.Logging (LoggerT, runLoggerT)
import Test.Spec (SpecT, beforeAll_, hoistSpec)

type LoggerSpecT m i m' a = SpecT (LoggerT m) i (LoggerT m') a

hoistLoggerSpec
  :: forall m' i m a
   . Monad m'
  => MonadEffect m'
  => MonadEffect m
  => MonadAff m
  => MonadError Error m
  => String
  -> LoggerSpecT m i m' a
  -> SpecT m i m' a
hoistLoggerSpec logFilepath =
  hoistSpec runLoggerT' (\_ -> runLoggerT') <<<
    beforeAll_ setup

  where
  runLoggerT' :: forall m'' a'. MonadEffect m'' => LoggerT m'' a' -> m'' a'
  runLoggerT' = flip runLoggerT { filepath: logFilepath }

  setup :: LoggerT m Unit
  setup = do
    whenM (FS.exists logFilepath # liftEffect) do
      FS.rm logFilepath # liftEffect
