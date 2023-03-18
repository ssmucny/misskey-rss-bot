module Run (run) where

import qualified Data.Map.Strict    as M
import           Data.Text          (strip, toCaseFold, unwords)
import           Data.Time          (UTCTime)
import           Data.Yaml          as Y (encode)
import           Import
import           Misskey.Api        (getLatestPostDate, postNote)
import           Misskey.Mfm
import           Network.HTTP.Types (Status (statusCode, statusMessage))
import           RIO.List           (sortOn)
import           RSS                (getFeed)
import           Util

run :: RIO App [()]
run = do
  logInfo "We're inside the application! YAML successfully parsed, options read. Ready to read feeds."
  logDebug "verbose logging enabled..."
  appConfig <- config <$> ask
  let tagMapping = tagMap $ appConfig.tags
  logInfo $ "---Start YAML Config---" <> displayBytesUtf8 (Y.encode appConfig) <> "---End YAML Config---"

  -- One thread is created for each feed, and it will recursivly update, never returning
  forConcurrently appConfig.feeds $ updatePosts tagMapping []

tagMap :: [TagPair] -> HashtagMap
tagMap tagPairs = M.fromListWith (<>) $ addHash <$> tagPairs

type HashtagMap = M.Map Text Text

addHash :: TagPair -> (Text, Text)
addHash tagPair = (toCaseFold tagPair.category, "#" <> tagPair.tag)

updatePosts :: HashtagMap -> [Note] -> FeedConfig -> RIO App ()
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
  noteResponses <- forM (basicFormatter tagsMap <$> fromRight [] filteredPosts) $ postNote feedConfig.instanceUrl (Just feedConfig.userToken)

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
logLeft (Left e) = logError $ display e
logLeft _        = pure ()

logLeftWith :: (MonadIO m, MonadReader env m, HasLogFunc env, Display a) =>
  (t -> a) -> Either t b -> m ()
logLeftWith format (Left e) = logError . display . format $ e
logLeftWith _ _             = pure ()

calcLocalLastDate :: [Note] -> Maybe UTCTime -> Maybe UTCTime
calcLocalLastDate previousPosts manualDate =
  if null previousPosts then manualDate else Just $ getLastDateFromCache previousPosts


getLastDateFromCache :: [Note] -> UTCTime
getLastDateFromCache = foldr max fallbackTime . fmap noteCreatedAt
  where
    noteCreatedAt :: Note -> UTCTime
    noteCreatedAt note = note.createdAt


basicFormatter :: HashtagMap -> Post -> NewNote
basicFormatter tagmap post = NewNote
  { text = formatContents tagmap post
  , visibility = Public
  , visibleUserIds = []
  , cw = Nothing
  , localOnly = True
  , noExtractMentions = False
  , noExtractHashtags = False
  , noExtractEmojis = False
  , replyId = Nothing
  , renoteId = Nothing
  , channelId = Nothing
  , fileIds = Nothing
  , poll = Nothing }

formatContents :: HashtagMap -> Post -> Mfm
formatContents tagmap post = strip $ (big . bold $ post.title) <> "\n"
  <> post.content <> " " <> hashtags <> " " <> urlLink post.link Nothing
  where hashtags = Data.Text.unwords $ mapMaybe (tagmap M.!?) normalizedCategories
        normalizedCategories = toCaseFold <$> post.categories

