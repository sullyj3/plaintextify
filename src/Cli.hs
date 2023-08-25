module Cli 
  ( InputType(..)
  , OutputMode(..)
  , parseArgsTemp
  ) where

import System.Environment (getArgs)
import Options.Applicative

data InputType = InputUrls | InputHtmlFiles [FilePath]
data InputMode = InputStdin
data OutputMode = OutputStdout | OutputSingleFile String | OutputIndividualFiles

parseArgsTemp :: IO (InputType, OutputMode)
parseArgsTemp = do
  args <- getArgs
  case args of
    [] -> pure (InputUrls, OutputStdout)
    ("--html-files":files ) -> pure (InputHtmlFiles files, OutputStdout)
    _ -> error "invalid args"

data Options = Options
  { optOutputMode :: OutputMode
  }

options :: Parser Options
options = Options <$> outputMode

outputMode :: Parser OutputMode
outputMode = -- flags are mutually exclusive
  outputIndividualFiles <|> outputSingleFile <|> outputStdout

outputIndividualFiles :: Parser OutputMode
outputIndividualFiles = flag' OutputIndividualFiles
  ( long "individual-files"
 <> short 'i'
 <> help "Output each page to a separate file" )

outputSingleFile :: Parser OutputMode
outputSingleFile = OutputSingleFile <$> strOption
  ( long "output-file"
 <> short 'o'
 <> metavar "FILE"
 <> help "Output all pages to a single file" )

-- Stdout is default
outputStdout :: Parser OutputMode
outputStdout = pure OutputStdout
