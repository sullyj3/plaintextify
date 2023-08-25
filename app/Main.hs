{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language BlockArguments #-}
module Main (main) where

import Network.Wreq (get, responseBody, responseHeader)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as Enc
import Data.Text (Text)
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
import Data.Foldable
import Control.Monad
import System.Console.ANSI
import qualified Cli
import           Cli (InputType(..), OutputMode(..))


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


main :: IO ()
main = do
  -- todo add cli flag for mode
  (inputType, outputMode) <- Cli.parseArgsTemp

  -- get space separated urls from stdin
  -- TODO sanitize input (eg check for no urls supplied)
  urls <- T.words <$> T.getContents

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

  output outputMode plainPages

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


output :: OutputMode -> [Page Text] -> IO ()
output outputMode plainPages = case outputMode of
    OutputStdout -> do
      -- cat all pages to stdout
      T.putStrLn $ T.unlines $ map pageBody plainPages
    OutputSingleFile path -> do
      -- write all pages to a single file
      T.writeFile path $ T.unlines $ map pageBody plainPages
    OutputIndividualFiles -> do
      -- concurrently write each page to a file
      forConcurrently_ plainPages $ \(Page url content) -> do
        let filename = case T.splitOn "/" url of
              [] -> error "empty url"
              segments -> T.unpack (last segments) -<.> "txt"
        T.writeFile filename content
