module Misskey (
    getNotes
    , getMe
    , getLatestPostDate
    , postNote
    , NotePredicate(..)
    ) where

import RIO
import           Data.Aeson
import           Data.Scientific
import           Data.Text           (pack, head)
import           Import
import           Network.HTTP.Simple
import Data.Time (UTCTime)

data NotePredicate =
      Limit Integer -- ^Limit `elem` [1..100]
    | Since NoteId
    | Until NoteId
    | IsLocal Bool
    | IsReply Bool
    | IsRenote Bool
    | HasFiles Bool
    | IsPoll Bool

defaultMkRequest :: Request
defaultMkRequest
    = setRequestMethod "POST"
    $ setRequestSecure True
    $ setRequestPort 443
    $ defaultRequest

getNotes :: (MonadIO m) => Url -> Maybe MkToken -> [NotePredicate] -> m (Either MkError [Note])
getNotes (Url host) token flags = do
    let request
            = setRequestPath "/api/notes"
            $ setRequestHost (encodeUtf8 host) -- ^misskey.io
            $ setRequestBodyJSON (object $ userAuthToken token <> map predicateTerm flags)
            $ defaultMkRequest
    mkResponseEither <$> httpJSON request

predicateTerm :: IsString a => NotePredicate -> (a, Value)
predicateTerm (Limit n)          = ("limit", Number (scientific n 0))
predicateTerm (Since (NoteId i)) = ("sinceId", String i)
predicateTerm (Until (NoteId i)) = ("untilId", String i)
predicateTerm (IsLocal b)        = ("local", Bool b)
predicateTerm (IsReply b)        = ("reply", Bool b)
predicateTerm (IsRenote b)       = ("renote", Bool b)
predicateTerm (HasFiles b)       = ("withFiles", Bool b)
predicateTerm (IsPoll b)         = ("poll", Bool b)


userAuthToken :: Maybe MkToken -> [(Key, Value)]
userAuthToken Nothing            = []
userAuthToken (Just (MkToken t)) = [("i", String t)]

mkResponseEither :: (FromJSON b) => Response Value -> Either MkError b
mkResponseEither response = case getResponseStatusCode response of
        200 -> case fromJSON (getResponseBody response) of
            Error e   -> Left $ MkError "AESON_PARSE_ERROR" (pack e) ""
            Success b -> Right b
        _   -> case fromJSON (getResponseBody response) of
            Error e   -> Left $ MkError "AESON_PARSE_ERROR" (pack e) ""
            Success b -> Left b

getMe :: (MonadIO m) =>
    Url -> Maybe MkToken -> m (Either MkError UserDetails)
getMe (Url host) token = do
    let request
            = setRequestPath "/api/i"
            $ setRequestHost (encodeUtf8 host) -- ^misskey.io
            $ setRequestBodyJSON (object $ userAuthToken token)
            $ defaultMkRequest
    mkResponseEither <$> httpJSON request



getLatestPostDate :: MonadIO m =>
    Url -> Maybe MkToken -> m (Either MkError UTCTime)
getLatestPostDate host token = do
    me <- getMe host token
    notes <- case me of
        Left e -> pure $ Left e
        Right i -> getUserNotes host token i.id [Limit 1]
    return $ createdAt <$> (headEither =<< notes)
    where headEither [] = Left $ MkError "NO_NOTES_FOR_USER" "" ""
          headEither (n:_) = Right n


getUserNotes :: (MonadIO m) => Url -> Maybe MkToken -> UserId -> [NotePredicate] -> m (Either MkError [Note])
getUserNotes (Url host) token (UserId uid) flags = do
    let request
            = setRequestPath "/api/users/notes"
            $ setRequestHost (encodeUtf8 host) -- ^misskey.io
            $ setRequestBodyJSON (object $ userAuthToken token <> [("userId", String uid)] <> map predicateTerm flags)
            $ defaultMkRequest
    mkResponseEither <$> httpJSON request

postNote :: (MonadIO m) => Url -> MkToken -> PostParams -> Map Text Text -> Post -> m (Either MkError Note)
postNote (Url host) (MkToken token) postOptions tagMap post = undefined

