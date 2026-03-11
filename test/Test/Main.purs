module Test.Main where

import Prelude

import Effect (Effect)
import Test.RewriteSim as Test.RewriteSim
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

--------------------------------------------------------------------------------

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = do
  Test.RewriteSim.spec

