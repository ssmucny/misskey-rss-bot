module Misskey (
    notes
    , NoteFilters(..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific
import           Data.Text           (pack)
import           Import
import           Network.HTTP.Simple
import           RIO

data NoteFilters =
      Limit Integer -- ^Limit `elem` [1..100]
    | Since NoteId
    | Until NoteId
    | IsLocal Bool
    | IsReply Bool
    | IsRenote Bool
    | HasFiles Bool
    | IsPoll Bool

notes :: Url -> (Maybe MkToken) -> [NoteFilters] -> IO (Either MkError [Note])
notes (Url host) token flags = do
    let request
            = setRequestPath "/api/notes"
            $ setRequestHost (encodeUtf8 host) -- ^misskey.io
            $ setRequestMethod "POST"
            $ setRequestBodyJSON (object $ userAuthToken token <> map term flags)
            $ setRequestSecure True
            $ setRequestPort 443
            $ defaultRequest
    mkResponseEither <$> httpJSON request
    where term (Limit n)          = ("limit", Number (scientific n 0))
          term (Since (NoteId i)) = ("sinceId", String i)
          term (Until (NoteId i)) = ("untilId", String i)
          term (IsLocal b)        = ("local", Bool b)
          term (IsReply b)        = ("reply", Bool b)
          term (IsRenote b)       = ("renote", Bool b)
          term (HasFiles b)       = ("withFiles", Bool b)
          term (IsPoll b)         = ("poll", Bool b)

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
