{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
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
import System.IO (print)
import Data.Aeson.KeyMap (fromList)
import RIO.Vector (create)

ok :: Response a -> Bool
ok response = getResponseStatusCode response == 200

mkResponse :: Response (Either JSONException b) -> Either MkError b
mkResponse response = if ok response
    then mapLeft mkJSONError $ getResponseBody response
    else Left $ MkError "HTTP_ERROR" (pack . show $ getResponseStatus response) (pack . show $ getResponseStatusCode response)

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
    mkResponse <$> httpJSONEither request

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

mkJSONError :: JSONException -> MkError
mkJSONError jsonException = MkError "JSON_EXCEPTION" (pack . show $ jsonException) ""

getMe :: (MonadIO m) =>
    Url -> Maybe MkToken -> m (Either MkError UserDetails)
getMe (Url hostname) token = do
    let request
            = setRequestPath "/api/i"
            $ setRequestHost (encodeUtf8 hostname) -- ^misskey.io
            $ setRequestBodyJSON (object $ userAuthToken token)
            $ defaultMkRequest
    mkResponse <$> httpJSONEither request


getLatestPostDate :: MonadIO m =>
    Url -> Maybe MkToken -> m (Either MkError UTCTime)
getLatestPostDate hostname token = do
    me <- getMe hostname token
    notes <- case me of
        Left e  -> pure $ Left e
        Right i -> getUserNotes hostname token i.id [Limit 1]
    return $ noteCreatedAt <$> (headEither me =<< notes)
    where headEither user [] = Left $ MkError "NO_NOTES_FOR_USER" ("username=" <> getUsername user) ""
          headEither _ (n:_) = Right n
          noteCreatedAt :: Note -> UTCTime
          noteCreatedAt note = note.createdAt
          getUsername :: Either a UserDetails -> Text
          getUsername = either (const "") (\u -> u.username)


getUserNotes :: (MonadIO m) => Url -> Maybe MkToken -> UserId -> [NotePredicate] -> m (Either MkError [Note])
getUserNotes (Url hostname) token (UserId uid) flags = do
    let request
            = setRequestPath "/api/users/notes"
            $ setRequestHost (encodeUtf8 hostname) -- ^misskey.io
            $ setRequestBodyJSON (object $ userAuthToken token <> [("userId", String uid)] <> map predicateTerm flags)
            $ defaultMkRequest
    mkResponse <$> httpJSONEither request



postNote :: (MonadIO m) => Url -> Maybe MkToken -> NewNote -> m (Either MkError Note)
postNote (Url hostname) token note = do
    let request
            = setRequestPath "/api/notes/create"
            $ setRequestHost (encodeUtf8 hostname) -- ^misskey.io
            $ setRequestBodyJSON (toJSON note `mergeObjects` object (userAuthToken token))
            $ defaultMkRequest
    noteResponse <- mkResponse <$> httpJSONEither request
    return $ mapRight createdNote noteResponse
    
data PostNoteCreated = PostNoteCreated
 {createdNote :: Note } deriving (Show, Generic, ToJSON, FromJSON)

mergeObjects :: Value -> Value -> Object
mergeObjects (Object o1) (Object o2) = o1 <> o2
mergeObjects (Object o1) _ = o1
mergeObjects _ (Object o2) = o2
mergeObjects _ _ = fromList []

mapRight :: (t -> b) -> Either a t -> Either a b
mapRight function (Right value) = Right $ function value
mapRight _ (Left value) = Left value
