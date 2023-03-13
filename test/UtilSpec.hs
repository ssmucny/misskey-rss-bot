module UtilSpec (spec) where

import           Import
import           Test.Hspec
import           Util
import Data.Time ( UTCTime(utctDay) )
import Data.Time.Calendar.OrdinalDate ( toOrdinalDate )

spec :: Spec
spec = do
  describe "fallbackTime" $ do
    it "gives correct date" $ toOrdinalDate fallbackTime.utctDay `shouldBe` (1970, 1)

  describe "delaySeconds" $ do
    it "shorter delay returns first" $ race (delaySeconds 5) (delaySeconds 1) `shouldReturn` Right ()