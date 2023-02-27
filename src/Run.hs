module Run (run) where

import           Data.Yaml as Y
import           Import
import           RIO
import           RSS       (getFeed)

run :: RIO App ()
run = do
  logInfo "We're inside the application! YAML successfully parsed, options read. Ready to read feeds."
  logDebug "verbose logging enabled..."
  env <- ask

  logInfo $ "---Start YAML Config---" <> displayBytesUtf8 (encode env.config) <> "---End YAML Config---"
