module Test.RewriteSim where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (runWriterT)
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import RewriteSim (newNormalizationCtx, newNormalizationEnv, normalize)
import RewriteSim.Examples (examples)
import Test.RewriteSim.Example.DerivationsEx1 as Test.RewriteSim.Example.DerivationsEx1
import Test.RewriteSim.Example.ISLSCv0d2d2 as Test.RewriteSim.Example.ISLSCv0d2d2
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

--------------------------------------------------------------------------------

spec :: SpecT Aff Unit Aff Unit
spec = do
  examples # traverse_ \example -> do
    describe ("example " <> example.name) do
      example.tests # traverse_ \test -> do
        it test.name do
          result <- normalize test.input
            # runExceptT
            # flip runReaderT
                ( newNormalizationCtx { system: { name: example.name, rules: example.rules } } {}
                )
            # flip runStateT
                ( newNormalizationEnv { gas: 1000 } {}
                )
            # runWriterT
          case result of
            (Left _err /\ _env) /\ _trace -> fail $ "Error"
            (Right output /\ _env) /\ _trace -> shouldEqual output test.output

  Test.RewriteSim.Example.ISLSCv0d2d2.spec
  Test.RewriteSim.Example.DerivationsEx1.spec
