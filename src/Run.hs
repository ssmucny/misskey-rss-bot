module Run (run) where

import qualified Data.Map.Strict as M
import           Data.Text
import           Data.Yaml       as Y
import           Import

run :: RIO App [()]
run = do
  logInfo "We're inside the application! YAML successfully parsed, options read. Ready to read feeds."
  logDebug "verbose logging enabled..."
  appConfig <- config <$> ask
  let tagMapping = tagMap $ appConfig.tags
  logInfo $ "---Start YAML Config---" <> displayBytesUtf8 (encode appConfig) <> "---End YAML Config---"

  -- One thread is created for each feed, and it will recursivly update, never returning
  liftIO $ forConcurrently appConfig.feeds $ updatePosts tagMapping []

tagMap :: [TagPair] -> M.Map Text Text
tagMap tagPairs = M.fromListWith (<>) $ addHash <$> tagPairs

addHash :: TagPair -> (Text, Text)
addHash tagPair = (tagPair.category, " #" <> (toCaseFold tagPair.tag))

updatePosts :: (M.Map Text Text) -> [Post] -> FeedConfig -> IO ()
updatePosts tagsMap prevPosts feedConf = do
  undefined
  -- get the last date

  -- get the update feed

  -- filter the feed to get new posts

  -- post new notes to Misskey

  -- wait for some time before calling updatePosts again
