module RSSSpec (spec) where

import           Import
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Util

spec :: Spec
spec = do
    describe "parse feed Success" $ do
        it "feed same" $ 0 `shouldBe` 0
