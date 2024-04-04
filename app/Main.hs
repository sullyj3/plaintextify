{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language BlockArguments #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T


import qualified Iris
import qualified Paths_plaintextify as Autogen

import qualified Cli
import Plaintextify (plaintextify)
import qualified Data.String as String

newtype App a = App { unApp :: Iris.CliApp Cli.Options () a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Iris.CliEnv Cli.Options ())
    )

-- TODO use Pretty?
progDesc :: String
progDesc = String.unlines
  [ "Fetch and convert web pages to plain text"
  , ""
  , "urls to fetch are read from stdin"
  ]

appSettings :: Iris.CliEnvSettings Cli.Options ()
appSettings = Iris.defaultCliEnvSettings
  { Iris.cliEnvSettingsHeaderDesc = "Plaintextify"
  , Iris.cliEnvSettingsProgDesc = progDesc
  , Iris.cliEnvSettingsVersionSettings =
      Just (Iris.defaultVersionSettings Autogen.version)
          { Iris.versionSettingsMkDesc = ("Plaintextify v" <>)
          }
  , Iris.cliEnvSettingsCmdParser = Cli.optionsP
  }

main :: IO ()
main = Iris.runCliApp appSettings (unApp app)

app :: App ()
app = do
  Cli.Options { Cli.optOutputMode } <- Iris.asksCliEnv Iris.cliEnvCmd

  -- get space separated urls from stdin
  -- TODO sanitize input (eg check for no urls supplied)
  -- TODO factor out means of getting urls (stdin, cli args)
  urls <- liftIO $ T.words <$> T.getContents
  liftIO $ plaintextify urls optOutputMode
