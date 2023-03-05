module Run (run) where

import qualified Data.Map.Strict as M
import           Data.Text       (toCaseFold)
import           Data.Yaml       as Y
import           Import
import           Misskey
import Data.Time ( defaultTimeLocale, UTCTime )
import Data.Time.Format (parseTimeOrError)
import RSS (getFeed)
import Network.HTTP.Types (Status(statusCode, statusMessage))
import Util
import RIO.List (sortOn)

run :: RIO App [()]
run = do
  logInfo "We're inside the application! YAML successfully parsed, options read. Ready to read feeds."
  logDebug "verbose logging enabled..."
  appConfig <- config <$> ask
  let tagMapping = tagMap $ appConfig.tags
  logInfo $ "---Start YAML Config---" <> displayBytesUtf8 (encode appConfig) <> "---End YAML Config---"

  -- One thread is created for each feed, and it will recursivly update, never returning
  forConcurrently appConfig.feeds $ updatePosts tagMapping []

tagMap :: [TagPair] -> M.Map Text Text
tagMap tagPairs = M.fromListWith (<>) $ addHash <$> tagPairs


addHash :: TagPair -> (Text, Text)
addHash tagPair = (tagPair.category, " #" <> toCaseFold tagPair.tag)

updatePosts :: (M.Map Text Text) -> [Note] -> FeedConfig -> RIO App ()
updatePosts tagsMap prevPosts feedConfig = do
  let localLastDate = calcLocalLastDate prevPosts feedConfig.dateOverride
  lastDate <- case localLastDate of
    Just date -> pure $ Right date
    Nothing -> getLatestPostDate feedConfig.instanceUrl (Just feedConfig.userToken)
  logLeft lastDate

  -- get the updated feed
  newFeed <- getFeed feedConfig.rssUrl
  logLeftWith (\status -> "Error fetching feed: " <> displayShow feedConfig.rssUrl <> ":: code: " <> display status.statusCode <> ", message: " <> displayBS status.statusMessage) newFeed

  -- filter the feed to get new posts
  let filteredPosts = sortOn (.createdDate) <$> filterPosts lastDate newFeed

  -- post new notes to Misskey
  logInfo $ "Posting " <> (display . length $ fromRight [] filteredPosts) <> " new notes to " <> displayShow feedConfig.instanceUrl <> " from " <> displayShow feedConfig.rssUrl
  noteResponses <- forM (fromRight [] filteredPosts) $ postNote feedConfig.instanceUrl feedConfig.userToken feedConfig.postParams tagsMap

  let (postErrors, newNotes) = partitionEithers noteResponses
  forM_ postErrors $ logError . display
  logInfo $ (display . length) newNotes <> " new notes created for " <> displayShow feedConfig.instanceUrl <> " from " <> displayShow feedConfig.rssUrl

  -- wait for some time before calling updatePosts again
  let noteCache = if null newNotes then prevPosts else newNotes
  liftIO $ delaySeconds (feedConfig.refreshFrequencyMin * 60)
  updatePosts tagsMap noteCache feedConfig
  

filterPosts :: Either MkError UTCTime -> Either Status [Post] -> Either MkError [Post]
filterPosts lastDate' feed' = do
  lastDate <- lastDate'
  feed <- mapLeft (const $ MkError "" "" "") feed'
  pure $ filter (\p -> p.createdDate > lastDate) feed


logLeft :: (MonadIO m, MonadReader env m, HasLogFunc env, Display a) =>
 Either a b -> m ()
logLeft result = case result of
  Left e -> logError $ display e
  Right _ -> pure ()


logLeftWith :: (MonadIO m, MonadReader env m, HasLogFunc env, Display a) =>
  (t -> a) -> Either t b -> m ()
logLeftWith format result = case result of
  Left e -> logError $ display $ format e
  Right _ -> pure ()

calcLocalLastDate :: [Note] -> Maybe UTCTime -> Maybe UTCTime
calcLocalLastDate previousPosts manualDate =
  if null previousPosts then manualDate else Just $ getLastDateFromCache previousPosts


getLastDateFromCache :: [Note] -> UTCTime
getLastDateFromCache = foldr max fallbackTime . fmap createdAt

