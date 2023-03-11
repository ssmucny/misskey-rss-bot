{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Misskey.Types (
      MkError(..)
    , emptyError
    , Url(..)
    , ChannelId(..)
    , MkToken(..)
    , NoteId(..)
    , UserId(..)
    , FileId(..)
    , Md5Sum(..)
    , ByteSize(..)
    , FolderId(..)
    , NoteVisibility(..)
    , NotePredicate(..)
    , Note(..)
    , NewNote(..)
    , File(..)
    , User(..)
    , UserDetails(..)
    ) where

import RIO
import Data.Aeson
    ( encode,
      withText,
      FromJSON(parseJSON),
      Object,
      Value(String),
      ToJSON(toJSON) )
import           Data.Time.Clock      (UTCTime)
import Data.ByteString.Lazy (unpack)
import qualified Data.Foldable

data NewNote = NewNote
 { text :: Text
 , visibility :: NoteVisibility
 , visibleUserIds :: [UserId]
 , cw :: Maybe Text -- ^Content Warning
 , localOnly :: Bool
 , noExtractMentions :: Bool
 , noExtractHashtags :: Bool
 , noExtractEmojis :: Bool
 , replyId :: Maybe NoteId
 , renoteId :: Maybe NoteId
 , channelId :: Maybe ChannelId
 , fileIds :: Maybe [FileId]
 , poll :: Maybe Poll
 } deriving (Show, Generic, FromJSON, ToJSON)

newtype Url = Url { toText :: Text } deriving (Show, Generic, FromJSON, ToJSON)
newtype ChannelId = ChannelId { toText :: Text } deriving (Show, Generic, ToJSON, FromJSON)
newtype MkToken = MkToken { toText :: Text } deriving (Show, Generic, FromJSON, ToJSON)
newtype NoteId = NoteId { toText :: Text } deriving (Show, Generic, FromJSON, ToJSON)
newtype UserId = UserId { toText :: Text } deriving (Show, Generic, FromJSON, ToJSON)
newtype FileId = FileId { toText :: Text } deriving (Show, Generic, FromJSON, ToJSON)
newtype Md5Sum = Md5Sum { toText :: Text } deriving (Show, Generic, FromJSON, ToJSON)
newtype ByteSize = ByteSize { toInteger :: Integer } deriving (Show, Generic, FromJSON, ToJSON)
newtype FolderId = FolderId { toText :: Text } deriving (Show, Generic, FromJSON, ToJSON)

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

data Poll = Poll
  { choices :: [Text] -- ^Min 2 items, Max 10, each choice has max length of 50 characters
  , multiple :: Bool -- ^Specifies if user can select more than one choice
  , expiresAt :: Maybe Integer
  , expiresAfter :: Maybe Integer
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

data NotePredicate =
      Limit Integer -- ^Limit `elem` [1..100]
    | Since NoteId
    | Until NoteId
    | IsLocal Bool
    | IsReply Bool
    | IsRenote Bool
    | HasFiles Bool
    | IsPoll Bool

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