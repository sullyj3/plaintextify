{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language BlockArguments #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Foldable
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.Wreq (get, responseBody, responseHeader)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Data.Text.Encoding as Enc
import qualified Text.Pandoc as Pandoc
import System.FilePath
import Lens.Micro
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Text.Html.Encoding.Detection as HtmlEncoding
import qualified Data.Text.ICU.Convert as ICUConvert
import Data.Char (toLower)
import System.IO (stderr)
import Control.Monad
import System.Console.ANSI

import qualified Iris
import qualified Paths_plaintextify as Autogen

import qualified Cli

data Page a = Page
  { pageUrl :: Text
  , pageBody :: a
  }

fetchPage :: Text -> IO (Page LBS.ByteString)
fetchPage url = do
  response <- get $ T.unpack url

  -- expect html
  let content_type :: ByteString
      content_type = response ^. responseHeader "Content-Type"
  case BS.take 9 content_type of
    "text/html" -> pure ()
    _ -> error $ "expected html, got " <> show content_type

  let lbody = response ^. responseBody
  pure $ Page url lbody

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
progDesc = unlines 
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


-- TODO refactor this into the App monad
plaintextify :: [Text] -> Cli.OutputMode -> IO ()
plaintextify urls optOutputMode = do

    -- lock for stderr to prevent concurrent output from being interleaved
    stderrLock <- newMVar ()

    let nToFetch = length urls

        showNFetched :: Int -> Text
        showNFetched n = "( " <> T.pack (show n) <> "/" <> T.pack (show nToFetch) <> " )"

        concurrentLogStderr :: Text -> IO ()
        concurrentLogStderr msg = withMVar stderrLock \_ -> T.hPutStrLn stderr msg

    T.hPutStrLn stderr "fetching and converting pages..."
    for_ urls \url -> T.hPutStrLn stderr $ "  " <> url
    T.hPutStrLn stderr $ showNFetched 0

    -- track how many pages have been fetched and processed
    nFetchedVar <- newMVar 0

    -- concurrently fetch and convert pages
    (plainPages :: [Page Text]) <- forConcurrently urls \url -> do
      -- TODO slow down requests to avoid being rate limited
      plainTextPage <- (toPlainTextPage <=< decodePage concurrentLogStderr <=< fetchPage) url
      nFetched <- modifyMVar nFetchedVar \n -> pure (n+1, n+1)
      withMVar stderrLock \_ -> do
        hCursorUpLine stderr 1
        hClearLine stderr
        T.hPutStrLn stderr $ showNFetched nFetched
      pure plainTextPage

    T.hPutStrLn stderr "done."

    -- separate stderr output from stdout output
    -- TODO only print newline here if stdout is a terminal
    T.hPutStrLn stderr ""

    output optOutputMode plainPages

toPlainTextPage :: Page Text -> IO (Page Text)
toPlainTextPage (Page url bodyText) = Pandoc.runIOorExplode $ do
  pandoc <- Pandoc.readHtml Pandoc.def bodyText
  plain <- Pandoc.writePlain Pandoc.def pandoc
  pure $ Page url plain

decodePage :: (Text -> IO ()) -> Page LBS.ByteString -> IO (Page Text)
decodePage log (Page url lbody) = do
  -- detect charset and convert to unicode text. We want to be lenient here 
  -- to support older websites.
  let strictBody = BS.toStrict lbody
  bodyText <- case map toLower <$> HtmlEncoding.detect lbody of
    Just "utf-8" -> pure $ Enc.decodeUtf8Lenient strictBody
    Just "ascii" -> pure $ Enc.decodeUtf8Lenient strictBody
    Just "iso-8859-1" -> pure $ Enc.decodeLatin1 strictBody
    Just "windows-1252" -> do
      converter <- ICUConvert.open "CP1252" Nothing
      pure $ ICUConvert.toUnicode converter strictBody
    Just charset -> do
      -- TODO BUG: this will interact badly with progress counter
      log $
        "WARNING: unsupported charset " <> T.pack charset <> " for url " <>
          url <> ", attempting to decode as utf-8"
      pure $ Enc.decodeUtf8Lenient strictBody
    Nothing -> do
      log $
        "WARNING: could not detect charset for url " <> url <>
          ", attempting to decode as utf-8"
      pure $ Enc.decodeUtf8Lenient strictBody
  pure $ Page url bodyText


output :: Cli.OutputMode -> [Page Text] -> IO ()
output outputMode plainPages = case outputMode of
    Cli.OutputStdout -> do
      -- cat all pages to stdout
      T.putStrLn $ T.unlines $ map pageBody plainPages
    Cli.OutputSingleFile path -> do
      -- write all pages to a single file
      T.writeFile path $ T.unlines $ map pageBody plainPages
    Cli.OutputIndividualFiles -> do
      -- concurrently write each page to a file
      forConcurrently_ plainPages $ \(Page url content) -> do
        let filename = case T.splitOn "/" url of
              [] -> error "empty url"
              segments -> T.unpack (last segments) -<.> "txt"
        T.writeFile filename content


