module RSS (
getFeed
, feedType
) where

import           Data.Text                 (unpack)
import           Data.Time                 (defaultTimeLocale, parseTimeOrError,
                                            rfc822DateFormat)
import           Import
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status (Status, mkStatus)
import           Text.Feed.Import          (parseFeedSource)
import           Text.Feed.Types
import           Text.RSS.Syntax
import           Util

feedType :: IsString a => Feed -> a
feedType (AtomFeed _) = "AtomFeed"
feedType (RSSFeed _)  = "RSSFeed"
feedType (RSS1Feed _) = "RSS1Feed"
feedType (XMLFeed _)  = "XMLFeed"

getFeed :: (MonadThrow m, MonadIO m) => Url -> m (Either Status [Post])
getFeed (Url feedUrl) = do
    response <- httpLBS =<< parseRequest (unpack feedUrl)
    return $ case getResponseStatusCode response of
            200   -> case parseFeedSource $ getResponseBody response of
                Nothing   -> Left $ mkStatus 0 "Unable to parse feed"
                Just feed -> Right $ normalizeFeed feed
            _  -> Left $ getResponseStatus response


normalizeFeed :: Feed -> [Post]
normalizeFeed (AtomFeed _) = error "AtomFeed not implemented"
normalizeFeed (RSSFeed feed) =
    let items = rssItems . rssChannel $ feed in rss2ItemtoPost <$> items
normalizeFeed (RSS1Feed _) = error "RSS1Feed not implemented"
normalizeFeed (XMLFeed _) = error "XML feed not implemented"

rss2ItemtoPost :: RSSItem -> Post
rss2ItemtoPost item = Post
    { title = "" `fromMaybe` rssItemDescription item `fromMaybe` rssItemTitle item
    , link = Url $ "" `fromMaybe` rssItemLink item
    , createdDate = maybe fallbackTime (parseTimeOrError True defaultTimeLocale rfc822DateFormat . unpack) item.rssItemPubDate
    , categories = rssCategoryValue <$> rssItemCategories item
    , content = "" `fromMaybe` rssItemContent item }
    --, content = "" `fromMaybe` rssItemDescription item `fromMaybe` rssItemContent item }
