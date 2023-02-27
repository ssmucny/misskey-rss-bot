{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Types
  ( App (..)
  , Options (..)
  , AppConfig (..)
  , Url(..)
  ) where

import           Data.Time.Calendar.OrdinalDate
import           Data.Yaml
import           GHC.Generics
import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { verbose    :: !Bool
  , configFile :: !Text
  }

data App = App
  { logFunc        :: !LogFunc
  , processContext :: !ProcessContext
  , options        :: !Options
  , config         :: !AppConfig
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens logFunc (\x y -> x { logFunc = y })
instance HasProcessContext App where
  processContextL = lens processContext (\x y -> x { processContext = y })

data AppConfig =
  AppConfig {
    feeds :: [Feed]
  , tags  :: [TagPair]
  } deriving (Show, Generic, FromJSON, ToJSON)

data Feed =
    Feed {
     instanceUrl          :: Url
    , rssUrl              :: Url
    , userToken           :: MkToken
    , refreshFrequencyMin :: Minutes
    , dateOverride        :: Maybe Day
    , misskeyParams       :: MisskeyParams
    , postParams          :: PostParams
    }  deriving (Show, Generic, FromJSON, ToJSON)

newtype Url = Url Text deriving (Show, Generic, FromJSON, ToJSON)
newtype MkToken = MkToken Text deriving (Show, Generic, FromJSON, ToJSON)
newtype Minutes = Minutes Int deriving (Show, Generic, FromJSON, ToJSON)

data MisskeyParams =
    MisskeyParams {
        visibility :: NoteVisibility
    , localOnly    :: Bool
    , channel      :: Maybe ChannelId
    } deriving (Show, Generic, FromJSON, ToJSON)

newtype ChannelId = ChannelId Text deriving (Show, Generic, FromJSON, ToJSON)

data NoteVisibility = Public | Home | Followers | Specified deriving (Show, Generic)
instance FromJSON NoteVisibility where
    parseJSON = withText "Visibility" $ \v ->
        case v of
            "public"    -> pure Public
            "home"      -> pure Home
            "followers" -> pure Followers
            "specified" -> pure Specified
            _           -> fail "String is not a known visibility"
instance ToJSON NoteVisibility where
    toJSON Public    = String "public"
    toJSON Home      = String "home"
    toJSON Followers = String "followers"
    toJSON Specified = String "specified"

data PostParams =
    PostParams {
        tags    :: [TagPair]
        --noteFormat    :: Text
    } deriving (Show, Generic, FromJSON, ToJSON)

data TagPair =
  TagPair {
    tag      :: Text
  , category :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)


