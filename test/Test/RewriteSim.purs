module Test.RewriteSim where

import Prelude

import Effect.Aff (Aff)
import Test.RewriteSim.Example.DerivationsEx1 as Test.RewriteSim.Example.DerivationsEx1
import Test.Spec (SpecT)

--------------------------------------------------------------------------------

spec :: SpecT Aff Unit Aff Unit
spec = do
  -- examples # traverse_ \example -> do
  --   describe ("example " <> example.name) do
  --     example.tests # traverse_ \test -> do
  --       it test.name do
  --         result <- normalize test.input
  --           # runExceptT
  --           # flip runReaderT
  --               ( newNormalizationCtx { system: { name: example.name, rules: example.rules } } {}
  --               )
  --           # flip runStateT
  --               ( newNormalizationEnv { gas: 1000 } {}
  --               )
  --           # runWriterT
  --         case result of
  --           (Left _err /\ _env) /\ _trace -> fail $ "Error"
  --           (Right output /\ _env) /\ _trace -> shouldEqual output test.output

  -- Test.RewriteSim.Example.ISLSCv0d2d2.spec
  Test.RewriteSim.Example.DerivationsEx1.spec
