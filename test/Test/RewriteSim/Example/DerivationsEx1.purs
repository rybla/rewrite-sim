module Test.RewriteSim.Example.DerivationsEx1 where

import Prelude

import RewriteSim.Example.DerivationsEx1 (makeDerivationSystem)
import Test.Spec (Spec, before, describe, it)

spec :: Spec Unit
spec =
  describe "DerivationsEx1" do
    before makeDerivationSystem do
      it "rules" \_derivationSystem -> do
        pure unit
