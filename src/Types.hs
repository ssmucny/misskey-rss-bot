{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Types
  ( App (..)
  , AppOptions(..)
  , AppConfig(..)
  , TagPair(..)
  , FeedConfig(..)
  , Post(..)
  , module Misskey.Types
  ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Time.Clock (UTCTime)
import           RIO
import           RIO.Process     (HasProcessContext (..), ProcessContext)

import           Misskey.Types

-- | Command line arguments
data AppOptions = AppOptions
  { verbose    :: !Bool
  , configFile :: !Text
  }

data App = App
  { logFunc        :: !LogFunc
  , processContext :: !ProcessContext
  , options        :: !AppOptions
  , config         :: !AppConfig
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens logFunc (\x y -> x { logFunc = y })
instance HasProcessContext App where
  processContextL = lens processContext (\x y -> x { processContext = y })

data AppConfig =
  AppConfig {
    feeds :: [FeedConfig]
  , tags  :: [TagPair]
  } deriving (Show, Generic, FromJSON, ToJSON)

data FeedConfig =
    FeedConfig {
     instanceUrl          :: Url
    , rssUrl              :: Url
    , userToken           :: MkToken
    , refreshFrequencyMin :: Int
    , dateOverride        :: Maybe UTCTime
    , misskeyParams       :: MisskeyParams
    , postParams          :: PostParams
    }  deriving (Show, Generic, FromJSON, ToJSON)


data MisskeyParams =
    MisskeyParams {
      visibility :: NoteVisibility
    , localOnly    :: Bool
    , channel      :: Maybe ChannelId
    } deriving (Show, Generic, FromJSON, ToJSON)

data PostParams =
    PostParams {
        tags    :: [TagPair]
        --noteFormat    :: Text
    } deriving (Show, Generic, FromJSON, ToJSON)

data TagPair =
  TagPair {
    category :: Text
  , tag      :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)


data Post = Post
  { link        :: Url
  , title       :: Text
  , content     :: Text
  , categories  :: [Text]
  , createdDate :: UTCTime
  } deriving (Show, Generic)
