{-# LANGUAGE ApplicativeDo #-}
module Cli (InputType(..), OutputMode(..), Options(..), optionsP) where

import qualified Options.Applicative as Opt

data InputType = InputUrls | InputHtmlFiles [FilePath]
  deriving Show
-- data InputMode = InputStdin
data OutputMode = OutputStdout | OutputSingleFile String | OutputIndividualFiles
  deriving Show

data Options = Options
  { optInputType :: InputType
  , optOutputMode :: OutputMode
  }
  deriving Show

optionsP :: Opt.Parser Options
optionsP = do
  optInputType <- inputTypeP
  optOutputMode <- outputModeP
  pure Options {optInputType, optOutputMode}

-- Defaults to InputUrls. Specify --html-files <FILES> to use InputHtmlFiles
inputTypeP :: Opt.Parser InputType
inputTypeP = do
  -- TODO optparse applicative only supports one argument per option flag,
  -- like `prog --flag arg1 --flag arg2`.
  -- probably I should pass files as `strArgument`s

  -- mHtmlFiles <- optional $ Opt.strOption $ mconcat
  --   [ Opt.long "html-files"
  --   , Opt.metavar "FILES"
  --   , Opt.help "Space separated list of html files to process"
  --   ]
  pure InputUrls

outputModeP :: Opt.Parser OutputMode
outputModeP = pure OutputStdout
