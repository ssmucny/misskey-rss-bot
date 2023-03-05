module MisskeySpec (spec) where

import           Import
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Util

spec :: Spec
spec = do
  describe "plus2" $ do
    it "basic check" $ plus2 0 `shouldBe` 2
