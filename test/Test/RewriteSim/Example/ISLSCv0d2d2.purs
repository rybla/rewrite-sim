module Test.RewriteSim.Example.ISLSCv0d2d2 where

import Prelude

import Control.Monad.Except (runExceptT)
import RewriteSim.Example.ISLSCv0d2d2 (oneTermDer, oneTermDer', twoTermDer, twoTermDer', zeroCtx)
import RewriteSim.Utilities (runIdentity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "ISLSC-0.2.2" do
    it "oneTermDer == oneTermDer'" do
      shouldEqual
        (oneTermDer @Void { n: zeroCtx } # runExceptT # runIdentity)
        (oneTermDer' { n: zeroCtx } # pure)
    it "twoTermDer == twoTermDer'" do
      shouldEqual
        (twoTermDer @Void { n: zeroCtx } # runExceptT # runIdentity)
        (twoTermDer' { n: zeroCtx } # pure)
    pure unit
