{-# LANGUAGE ApplicativeDo #-}

module QBar.Cli where

import qualified Data.Text as T
import Options.Applicative

data BarCommand = BarServer | NoFilter | RainbowFilter

barCommandParser :: Parser BarCommand
barCommandParser = hsubparser
  ( command "server" (info (pure BarServer) (progDesc "Start a new qbar server. Should be called by swaybar, i3bar or or another i3bar-protocol compatible host process.")) <>
    command "default" (info (pure NoFilter) (progDesc "Send a message to a running qbar server.")) <>
    command "rainbow" (info (pure RainbowFilter) (progDesc "Send a message to a running qbar server."))
  )

data MainOptions = MainOptions {
  verbose :: Bool,
  socketLocation :: Maybe T.Text,
  barCommand :: BarCommand
}

mainOptionsParser :: Parser MainOptions
mainOptionsParser = do
  verbose <- switch $ long "verbose" <> short 'v' <> help "Print more diagnostic output to stderr (including a copy of every bar update)."
  socketLocation <- optional $ strOption $ long "socket" <> short 's' <> metavar "SOCKET" <> help "Control socket location. By default determined by WAYLAND_SOCKET location."
  barCommand <- barCommandParser
  return MainOptions {verbose, socketLocation, barCommand}

parser :: ParserInfo MainOptions
parser = info (mainOptionsParser <**> helper)
  (fullDesc <> header "q - queezles tools")

parserPrefs :: ParserPrefs
parserPrefs = prefs showHelpOnEmpty

parseOptions :: IO MainOptions
parseOptions = customExecParser parserPrefs parser