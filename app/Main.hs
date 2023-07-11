{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
module Main (main) where

import Network.Wreq (get, responseBody, responseHeader)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
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

data OutputMode = OutputStdout | OutputSingleFile String | OutputIndividualFiles

data Page a = Page
  { pageUrl :: Text
  , pageBody :: a
  }

main :: IO ()
main = do
  -- todo add cli flag for mode
  let mode = OutputStdout

  -- get newline separated urls from stdin
  urls <- T.lines <$> T.getContents

  -- create lock for stdout in order to prevent interleaved output
  lock <- newMVar ()

  -- fetch urls concurrently
  (pages :: [Page LBS.ByteString]) <- forConcurrently urls $ \url -> do
    withMVar lock $ \_ -> T.putStrLn $ "fetching " <> url
    response <- get $ T.unpack url

    -- expect html
    let content_type :: ByteString
        content_type = response ^. responseHeader "Content-Type"
    case BS.take 9 content_type of
      "text/html" -> pure ()
      _ -> error $ "expected html, got " <> show content_type

    let lbody = response ^. responseBody
    pure $ Page url lbody

  -- concurrently convert each html page to plain text
  (plainPages :: [Page Text]) <- forConcurrently pages $ \(Page url lbody) -> do
    -- detect charset and convert to unicode text. We want to be lenient here 
    -- to support older websites.
    bodyText <- case map toLower <$> HtmlEncoding.detect lbody of
      Just "utf-8" -> pure $ T.decodeUtf8Lenient $ BS.toStrict lbody
      Just "ascii" -> pure $ T.decodeUtf8Lenient $ BS.toStrict lbody
      Just "windows-1252" -> do
        converter <- ICUConvert.open "CP1252" Nothing
        let strictBody = BS.toStrict lbody
        pure $ ICUConvert.toUnicode converter strictBody
      -- TODO detect other charsets
      Just charset -> error $ "unsupported charset " <> show charset
      Nothing -> error $ "could not detect charset of url " <> T.unpack url
        
    -- convert html to plain text
    Pandoc.runIOorExplode $ do
      pandoc <- Pandoc.readHtml Pandoc.def bodyText
      plain <- Pandoc.writePlain Pandoc.def pandoc
      pure $ Page url plain

  case mode of
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
              segments -> (T.unpack $ last segments) -<.> "txt"
        T.writeFile filename content
