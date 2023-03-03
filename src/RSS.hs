module RSS (
getFeed
, feedType
) where

import           Data.Text                 (unpack)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           RIO
import           Text.Feed.Import          (parseFeedSource)
import           Text.Feed.Types
import           Types

feedType :: IsString a => Feed -> a
feedType (AtomFeed _) = "AtomFeed"
feedType (RSSFeed _)  = "RSSFeed"
feedType (RSS1Feed _) = "RSS1Feed"
feedType (XMLFeed _)  = "XMLFeed"

getFeed :: (MonadThrow m, MonadIO m) => Url -> m (Either Status Feed)
getFeed (Url url) = do
    response <- httpLBS =<< parseRequest (unpack url)
    return $ case getResponseStatusCode response of
            200   -> case (parseFeedSource $ getResponseBody response) of
                Nothing   -> Left $ mkStatus 0 "Unable to parse feed"
                Just feed -> Right feed
            _  -> Left $ getResponseStatus response
