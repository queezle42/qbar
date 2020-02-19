{-# LANGUAGE ApplicativeDo #-}

module QBar.Cli where

import QBar.Blocks
import QBar.Blocks.Pipe
import QBar.ControlSocket
import QBar.Core
import QBar.DefaultConfig
import QBar.Server
import QBar.Theme

import Control.Monad (join, sequence_)
import qualified Data.Text.Lazy as T
import Options.Applicative

-- |Entry point.
runQBar :: IO ()
runQBar = join parseMain

parseMain :: IO (IO ())
parseMain = customExecParser parserPrefs parser
  where
    parser :: ParserInfo (IO ())
    parser = info (mainParser <**> helper)
      (fullDesc <> header "qbar - queezles {i3,sway}bar infrastructure")

    parserPrefs :: ParserPrefs
    parserPrefs = prefs showHelpOnEmpty

mainParser :: Parser (IO ())
mainParser = do
  verbose <- switch $ long "verbose" <> short 'v' <> help "Print more diagnostic output to stderr (including a copy of every bar update)."
  indicator <- switch $ long "indicator" <> short 'i' <> help "Show render indicator."
  socketLocation <- optional $ strOption $ long "socket" <> short 's' <> metavar "SOCKET" <> help "Control socket location. By default determined by WAYLAND_SOCKET location."
  barCommand <- barCommandParser
  return (barCommand MainOptions {verbose, indicator, socketLocation})

barCommandParser :: Parser (MainOptions -> IO ())
barCommandParser = hsubparser (
    command "server" (info (runBarServer <$> barConfigurationParser) (progDesc "Start a new qbar server. Should be called by swaybar, i3bar or or another i3bar-protocol compatible host process.")) <>
    command "connect" (info (sendBlockStream <$> barConfigurationParser) (progDesc "Run blocks on this process but display them on the qbar server.")) <>
    command "pipe" (info pipeClientParser (progDesc "Redirects the stdin of this process to a running bar.")) <>
    command "theme" (info themeCommandParser (progDesc "Change the theme of the running qbar server.")) <>
    command "default" (info (pure $ sendIpc . SetTheme $ "default") (progDesc "Shortcut for 'qbar theme default'.")) <>
    command "rainbow" (info (pure $ sendIpc . SetTheme $ "rainbow") (progDesc "Shortcut for 'qbar theme rainbow'."))
  )

themeCommandParser :: Parser (MainOptions -> IO ())
themeCommandParser = sendIpc . SetTheme <$> strArgument (metavar "THEME" <> completeWith (map T.unpack themeNames))

pipeClientParser :: Parser (MainOptions -> IO ())
pipeClientParser = do
  events <- switch $ long "events" <> short 'e' <> help "Also encode events to stdout. Every event will be a JSON-encoded line."
  pure $ runPipeClient events

barConfigurationParser :: Parser (BarIO ())
barConfigurationParser = do
  blocks <- many blockParser
  pure $ case blocks of
    -- Load default config if no blocks are selected on the command line
    [] -> defaultBarConfig
    l -> sequence_ l

blockParser :: Parser (BarIO ())
blockParser = hsubparser (
    command "default" (info (pure defaultBarConfig) (progDesc "Load default set of blocks.")) <>
    command "date" (info (pure $ addBlock dateBlock) (progDesc "Load the date and time block."))
  )
