{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Data.Text                  (unpack)
import qualified Data.Yaml                  as Y
import           Import
import           Options.Applicative.Simple
import qualified Paths_misskey_rss_bot
import           RIO.Process
import           Run

main :: IO [()]
main = do
  (cmdOptions, ()) <- simpleOptions
    $(simpleVersion Paths_misskey_rss_bot.version)
    "Usage of misskey-rss-bot"
    "A bot that collects RSS feeds and posts them to Misskey"
    (AppOptions
       <$> switch (
          long "verbose"
          <> short 'v'
          <> help "Verbose output?"
        )
       <*> strOption (
          long "config"
          <> short 'c'
          <> help "YAML configuration file for RSS bot"
          <> metavar "CONFIG_FILE"
          <> noArgError (ErrorMsg "Configuration file required to run. Use --config to specify.")
        )
    )
    empty
  lo <- logOptionsHandle stderr cmdOptions.verbose -- ^Set verbose logging
  pc <- mkDefaultProcessContext
  configContents <- Y.decodeFileThrow (unpack cmdOptions.configFile) -- ^read in YAML config file and parse
  withLogFunc lo $ \lf ->
    let app = App
          { logFunc = lf
          , processContext = pc
          , options = cmdOptions
          , config = configContents
          }
     in runRIO app run
