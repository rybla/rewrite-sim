module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Test.RewriteSim as Test.RewriteSim
import Test.Spec (SpecT)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (defaultConfig)

--------------------------------------------------------------------------------

main :: Effect Unit
main = runSpecAndExitProcess'
  { defaultConfig: defaultConfig
  , parseCLIOptions: true
  }
  [ consoleReporter ]
  spec

spec :: SpecT Aff Unit Aff Unit
spec = do
  Test.RewriteSim.spec
