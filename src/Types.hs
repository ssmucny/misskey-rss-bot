{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Types
  ( App (..)
  , AppOptions (..)
  , AppConfig (..)
  , Url(..)
  , TagPair(..)
  , FeedConfig(..)
  , MkToken(..)
  , Note(..)
  , NoteId(..)
  , MkError(..)
  , User(..)
  , UserId(..)
  , UserDetails(..)
  , Post(..)
  , PostParams(..)
  , emptyError
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy (unpack)
import qualified Data.Foldable
import           Data.Time.Clock      (UTCTime)
import           RIO
import           RIO.Process          (HasProcessContext (..), ProcessContext)

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

newtype Url = Url Text deriving (Show, Generic, FromJSON, ToJSON)
newtype MkToken = MkToken Text deriving (Show, Generic, FromJSON, ToJSON)

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
    category :: Text
  , tag      :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data MkError = MkError
    { code    :: Text
    , message :: Text
    , id      :: Text
    } deriving (Show, Generic, FromJSON, ToJSON)
emptyError :: MkError
emptyError = MkError "" "" ""

instance Display MkError where
  display = Data.Foldable.fold . fmap display . unpack . encode

data Note = Note
    { id           :: NoteId
    , createdAt    :: UTCTime
    , text         :: Maybe Text
    , cw           :: Maybe Text
    , userId       :: UserId
    , mentions     :: Maybe [UserId]
    , user         :: User
    , replyId      :: Maybe NoteId
    , reply        :: Maybe Note
    , renoteId     :: Maybe NoteId
    , renote       :: Maybe Note
    , visibility   :: NoteVisibility
    , localOnly    :: Bool
    , renoteCount  :: Int
    , repliesCount :: Int
    --, reactions :: ()
    --, reactionEmojis :: ()
    --, emojis :: ()
    , fileIds      :: [FileId]
    , files        :: [File]
    , uri          :: Maybe Url
    , tags         :: Maybe [Text]
    } deriving (Show, Generic, FromJSON, ToJSON)

data File = File
 { id           :: FileId
 , createdAt    :: UTCTime
 , name         :: Text
 --, type :: MimeType
 , md5          :: Md5Sum
 , size         :: ByteSize
 , isSensitive  :: Bool
 , blurhash     :: Text
 , properties   :: Object
 , url          :: Url
 , thumbnailUrl :: Url
 , comment      :: Maybe Text
 , folderId     :: Maybe FolderId
 , userId       :: Maybe UserId
 , user         :: Maybe User
 }  deriving (Show, Generic, FromJSON, ToJSON)

data User = User
  { id       :: UserId
  , name     :: Text
  , username :: Text
  , host     :: Maybe Url
  } deriving (Show, Generic, FromJSON, ToJSON)

data UserDetails = UserDetails
  { id            :: UserId
  , name          :: Text
  , username      :: Text
  , host          :: Maybe Url
  , notesCount    :: Int
  , pinnedNoteIds :: [NoteId]
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype NoteId = NoteId Text deriving (Show, Generic, FromJSON, ToJSON)
newtype UserId = UserId Text deriving (Show, Generic, FromJSON, ToJSON)
newtype FileId = FileId Text deriving (Show, Generic, FromJSON, ToJSON)
newtype Md5Sum = Md5Sum Text deriving (Show, Generic, FromJSON, ToJSON)
newtype ByteSize = ByteSize Integer deriving (Show, Generic, FromJSON, ToJSON)
newtype FolderId = FolderId Text deriving (Show, Generic, FromJSON, ToJSON)

data Post = Post
  { link        :: Url
  , title       :: Text
  , content     :: Text
  , categories  :: [Text]
  , createdDate :: UTCTime
  } deriving (Show, Generic)
