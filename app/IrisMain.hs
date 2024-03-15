{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module IrisMain (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Foldable
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Applicative as Opt
import qualified System.Console.Pretty as Pretty

import qualified Iris

import qualified Paths_plaintextify as Autogen

data Options = Options
  { optionsFile :: FilePath
  , optionsSearch :: Text
  }

optionsP :: Opt.Parser Options
optionsP = Options <$> file <*> search
  where
    file = Opt.strOption $ mconcat
      [ Opt.long "file"
      , Opt.short 'f'
      , Opt.metavar "FILE_PATH"
      , Opt.help "Path to the file to search"
      ]
    search = Opt.strOption $ mconcat
      [ Opt.long "search"
      , Opt.short 's'
      , Opt.metavar "STRING"
      , Opt.help "Substring to find and highlight"
      ]

newtype App a = App { unApp :: Iris.CliApp Options () a }
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Iris.CliEnv Options ())
    )

appSettings :: Iris.CliEnvSettings Options ()
appSettings = Iris.defaultCliEnvSettings
  { Iris.cliEnvSettingsHeaderDesc = "Iris usage example"
  , Iris.cliEnvSettingsProgDesc = "Search for a substring in a file"
  , Iris.cliEnvSettingsVersionSettings = 
      Just (Iris.defaultVersionSettings Autogen.version)
          { Iris.versionSettingsMkDesc = \v -> "Simple grep utility v" <> v 
          }
  , Iris.cliEnvSettingsCmdParser = optionsP
  }

getFileContent :: FilePath -> App Text
getFileContent = liftIO . T.readFile

search :: Text -> Text -> [(Int, Text)]
search str
  = filter (\(_i, line) -> str `T.isInfixOf` line)
  . zip [1..]
  . T.lines

output :: [(Int, Text)] -> App ()
output = traverse_ outputLine
  where
    outputLine :: (Int, Text) -> App ()
    outputLine (i, line) = do
      outputLineNumber i
      liftIO $ T.putStrLn line

    outputLineNumber :: Int -> App ()
    outputLineNumber i = Iris.putStderrColoured
      (Pretty.color Pretty.Yellow . Pretty.style Pretty.Bold)
      (T.pack $ show i <> ": ")
  
app :: App ()
app = do
  Options {..} <- Iris.asksCliEnv Iris.cliEnvCmd
  fileContent <- getFileContent optionsFile
  let searchResult = search optionsSearch fileContent
  output searchResult

main :: IO ()
main = Iris.runCliApp appSettings (unApp app)
        
