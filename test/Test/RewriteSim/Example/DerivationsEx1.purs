module Test.RewriteSim.Example.DerivationsEx1 where

import Prelude

import RewriteSim.Example.DerivationsEx1 (derivationSystem)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  describe "DerivationsEx1" do
    it "ex1" do
      let _ = derivationSystem.rules "Lam"
      pure unit

