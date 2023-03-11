module Misskey.Api (
    getNotes
    , getMe
    , getLatestPostDate
    , postNote
    , NotePredicate(..)
    ) where

import RIO
import           Data.Aeson
import           Data.Scientific     (scientific)
import           Data.Text           (pack)
import           Data.Time           (UTCTime)
import           Network.HTTP.Simple

import Misskey.Types



defaultMkRequest :: Request
defaultMkRequest
    = setRequestMethod "POST"
    $ setRequestSecure True
    $ setRequestPort 443
    $ defaultRequest

getNotes :: (MonadIO m) => Url -> Maybe MkToken -> [NotePredicate] -> m (Either MkError [Note])
getNotes (Url hostname) token flags = do
    let request
            = setRequestPath "/api/notes"
            $ setRequestHost (encodeUtf8 hostname) -- ^misskey.io
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
getMe (Url hostname) token = do
    let request
            = setRequestPath "/api/i"
            $ setRequestHost (encodeUtf8 hostname) -- ^misskey.io
            $ setRequestBodyJSON (object $ userAuthToken token)
            $ defaultMkRequest
    mkResponseEither <$> httpJSON request



getLatestPostDate :: MonadIO m =>
    Url -> Maybe MkToken -> m (Either MkError UTCTime)
getLatestPostDate hostname token = do
    me <- getMe hostname token
    notes <- case me of
        Left e  -> pure $ Left e
        Right i -> getUserNotes hostname token i.id [Limit 1]
    return $ noteCreatedAt <$> (headEither =<< notes)
    where headEither []    = Left $ MkError "NO_NOTES_FOR_USER" "" ""
          headEither (n:_) = Right n
          noteCreatedAt :: Note -> UTCTime
          noteCreatedAt note = note.createdAt


getUserNotes :: (MonadIO m) => Url -> Maybe MkToken -> UserId -> [NotePredicate] -> m (Either MkError [Note])
getUserNotes (Url hostname) token (UserId uid) flags = do
    let request
            = setRequestPath "/api/users/notes"
            $ setRequestHost (encodeUtf8 hostname) -- ^misskey.io
            $ setRequestBodyJSON (object $ userAuthToken token <> [("userId", String uid)] <> map predicateTerm flags)
            $ defaultMkRequest
    mkResponseEither <$> httpJSON request



postNote :: (MonadIO m) => Url -> Maybe MkToken -> NewNote -> m (Either MkError Note)
postNote (Url hostname) token note = do
    let request
            = setRequestPath "/api/notes/create"
            $ setRequestHost (encodeUtf8 hostname) -- ^misskey.io
            $ setRequestBodyJSON (toJSONList [toJSON note, object (userAuthToken token)])
            $ defaultMkRequest
    mkResponseEither <$> httpJSON request

