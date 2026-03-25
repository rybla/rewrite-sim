module Test.RewriteSim.Example.ISLSCv0d2d2 where

import Prelude

import Control.Monad.Except (runExceptT)
import RewriteSim.Example.Common (me)
import RewriteSim.Example.ISLSCv0d2d2 (deBruijn, oneDer, oneDer', twoDer, twoDer', zeroDer)
import RewriteSim.Utilities (runIdentity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "ISLSC-0.2.2" do
    it "oneDer == oneDer'" do
      shouldEqual
        (oneDer { n: me "n" } # runExceptT # runIdentity)
        (oneDer' { n: me "n" } # pure)
    it "twoDer == twoDer'" do
      shouldEqual
        (twoDer { n: me "n" } # runExceptT # runIdentity)
        (twoDer' { n: me "n" } # pure)
    it "zeroDer == deBruijn 0" do
      shouldEqual
        (zeroDer { n: me "n" } # pure)
        (deBruijn 0 { n: me "n" } # runExceptT # runIdentity)
    it "oneDer == deBruijn 1" do
      shouldEqual
        (oneDer { n: me "n" } # runExceptT # runIdentity)
        (deBruijn 1 { n: me "n" } # runExceptT # runIdentity)
    it "twoDer == deBruijn 2" do
      shouldEqual
        (twoDer { n: me "n" } # runExceptT # runIdentity)
        (deBruijn 2 { n: me "n" } # runExceptT # runIdentity)
    pure unit
