{-# LANGUAGE ApplicativeDo #-}

module QBar.Cli where

import QBar.Blocks
import QBar.Blocks.Pipe
import QBar.ControlSocket
import QBar.Core
import QBar.DefaultConfig
import QBar.Server
import QBar.Theme

import Control.Monad (join)
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
    command "server" (info serverCommandParser (progDesc "Start a new server.")) <>
    command "pipe" (info pipeClientParser (progDesc "Redirects the stdin of this process to a running bar.")) <>
    command "theme" (info themeCommandParser (progDesc "Change the theme of the running qbar server."))
  )

serverCommandParser :: Parser (MainOptions -> IO ())
serverCommandParser = hsubparser (
    command "swaybar" (info (runBarServer <$> barConfigurationParser) (progDesc "Start a new server for swaybar. Should be called by swaybar.")) <>
    command "i3bar" (info (runBarServer <$> barConfigurationParser) (progDesc "Start a new server for i3bar. Should be called by i3bar.")) <>
    command "connect" (info (sendBlockStream <$> barConfigurationParser) (progDesc "Run blocks on this process but send them to another qbar server."))
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
