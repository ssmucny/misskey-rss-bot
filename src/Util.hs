-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , displayBS
  , fallbackTime
  , delaySeconds
  ) where

import           Data.ByteString as BS (unpack)
import qualified Data.Foldable
import           Data.Time       (UTCTime, defaultTimeLocale, parseTimeOrError)
import           Import

plus2 :: Int -> Int
plus2 = (+ 2)


displayBS :: ByteString -> Utf8Builder
displayBS = Data.Foldable.fold . fmap display . unpack

fallbackTime :: UTCTime
fallbackTime = understandTime "1970-01-01"
  where timeFormat = "%Y-%m-%d"
        understandTime = parseTimeOrError True defaultTimeLocale timeFormat

delaySeconds :: Int -> IO ()
delaySeconds sec
  | sec <= thousandSec = threadDelay $ sec * 1000000
  | otherwise = threadDelay (thousandSec * 1000000) >> delaySeconds (sec - thousandSec)
  where thousandSec = 1000
