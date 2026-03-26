module Test.RewriteSim.Example.ISLSCv0d2d2 where

import Prelude

import Control.Monad.Except (runExceptT)
import RewriteSim.Example.Common (me)
import RewriteSim.Example.ISLSCv0d2d2 (deBruijn_v1, fromMetaGenericTermDerExpr, oneDer_v0, oneDer_v1, oneDer_v2, twoDer_v0, twoDer_v1, twoDer_v2, zeroDer, zeroDerM)
import RewriteSim.Utilities (runIdentity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "ISLSC-0.2.2" do

    it "oneDer_v1 == oneDer_v0" do
      shouldEqual
        (oneDer_v1 { n: me "n" } # runExceptT # runIdentity)
        (oneDer_v0 { n: me "n" } # pure)
    it "twoDer_v1 == twoDer_v0" do
      shouldEqual
        (twoDer_v1 { n: me "n" } # runExceptT # runIdentity)
        (twoDer_v0 { n: me "n" } # pure)

    it "zeroDer == deBruijn 0" do
      shouldEqual
        (zeroDer { n: me "n" } # pure)
        (deBruijn_v1 0 { n: me "n" } # runExceptT # runIdentity)
    it "oneDer_v1 == deBruijn 1" do
      shouldEqual
        (oneDer_v1 { n: me "n" } # runExceptT # runIdentity)
        (deBruijn_v1 1 { n: me "n" } # runExceptT # runIdentity)
    it "twoDer_v1 == deBruijn 2" do
      shouldEqual
        (twoDer_v1 { n: me "n" } # runExceptT # runIdentity)
        (deBruijn_v1 2 { n: me "n" } # runExceptT # runIdentity)

    it "zeroDer == zeroDerM" do
      shouldEqual
        (zeroDer { n: me "n" } # pure)
        (zeroDerM { n: me "n" } # map fromMetaGenericTermDerExpr # runExceptT # runIdentity)
    it "oneDer == oneDerM" do
      shouldEqual
        (oneDer_v0 { n: me "n" } # pure)
        (oneDer_v2 { n: me "n" } # map fromMetaGenericTermDerExpr # runExceptT # runIdentity)
    it "zeroDer == zeroDerM" do
      shouldEqual
        (twoDer_v0 { n: me "n" } # pure)
        (twoDer_v2 { n: me "n" } # map fromMetaGenericTermDerExpr # runExceptT # runIdentity)

    pure unit
