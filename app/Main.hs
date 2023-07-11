{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
module Main (main) where

import Network.Wreq (get, responseBody, responseHeader)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Text.Pandoc
import System.FilePath
import Lens.Micro ((^.), (^?))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

data OutputMode = OutputStdout | OutputSingleFile String | OutputIndividualFiles

main = do
  -- todo add cli flag for mode
  let mode = OutputStdout

  -- get newline separated urls from stdin
  urls <- T.lines <$> T.getContents

  -- create lock for stdout in order to prevent interleaved output
  lock <- newMVar ()

  -- fetch urls concurrently
  (pages :: [(FilePath, Text)]) <- forConcurrently urls $ \url -> do
    withMVar lock $ \_ -> T.putStrLn $ "fetching " <> url
    response <- get $ T.unpack url
    let filename = case T.splitOn "/" url of
          [] -> error "empty url"
          segments -> (T.unpack $ last segments) -<.> "txt"

    -- expect html
    let content_type :: ByteString
        content_type = response ^. responseHeader "Content-Type"
    case BS.take 9 content_type of
      "text/html" -> pure ()
      _ -> error $ "expected html, got " <> show content_type

    -- TODO do proper charset detection
    let body = T.decodeUtf8Lenient $ BS.toStrict $ response ^. responseBody
    pure (filename, body)

  -- concurrently convert each html page to plain text
  plainPages <- forConcurrently pages $ \(filename, body) -> runIOorExplode $ do
    pd <- readHtml def body
    plain <- writePlain def pd
    pure (filename, plain)

  case mode of
    OutputStdout -> do
      -- cat all pages to stdout
      T.putStrLn $ T.unlines $ map snd plainPages
    OutputSingleFile path -> do
      -- write all pages to a single file
      T.writeFile path $ T.unlines $ map snd plainPages
    OutputIndividualFiles -> do
      -- concurrently write each page to a file
      forConcurrently_ plainPages $ \(filename, content) -> do
        T.writeFile filename content
