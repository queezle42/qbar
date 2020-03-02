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
    command "mirror" (info mirrorCommandParser (progDesc "Mirror the output of a running server.")) <>
    command "pipe" (info pipeClientParser (progDesc "Redirects the stdin of this process to a running bar.")) <>
    command "theme" (info themeCommandParser (progDesc "Change the theme of the running qbar server."))
  )

serverCommandParser :: Parser (MainOptions -> IO ())
serverCommandParser = hsubparser (
    command "swaybar" (info (runBarServer <$> barConfigurationParser) (progDesc "Start a new server. Should be called by swaybar.")) <>
    command "i3bar" (info (runBarServer <$> barConfigurationParser) (progDesc "Start a new server. Should be called by i3bar.")) <>
    command "send" (info (sendBlockStream <$> barConfigurationParser) (progDesc "Run blocks on this process but send them to another qbar server."))
  )
  where
    barConfigurationParser :: Parser (BarIO ())
    barConfigurationParser = sequence_ <$> some blockParser

mirrorCommandParser :: Parser (MainOptions -> IO ())
mirrorCommandParser = hsubparser (
    command "swaybar" (info (runBarServerMirror <$> barConfigurationParser) (progDesc "Mirror the output of another server. Should be called by swaybar.")) <>
    command "i3bar" (info (runBarServerMirror <$> barConfigurationParser) (progDesc "Mirror the output of another server. Should be called by i3bar."))
  )
  where
    barConfigurationParser :: Parser (BarIO ())
    barConfigurationParser = sequence_ <$> many blockParser


themeCommandParser :: Parser (MainOptions -> IO ())
themeCommandParser = sendIpc . SetTheme <$> strArgument (metavar "THEME" <> completeWith (map T.unpack themeNames))

pipeClientParser :: Parser (MainOptions -> IO ())
pipeClientParser = do
  events <- switch $ long "events" <> short 'e' <> help "Also encode events to stdout. Every event will be a JSON-encoded line."
  pure $ runPipeClient events

blockParser :: Parser (BarIO ())
blockParser =
  subparser (
    commandGroup "Available presets:" <>
    metavar "CONFIG..." <>
    command "default" (info (pure defaultBarConfig) (progDesc "Load default set of blocks.")) <>
    command "legacy" (info (pure legacyBarConfig) (progDesc "Load the legacy configuration. Requires some custom block scripts."))
  )
  <|>
  subparser (
    commandGroup "Available blocks:" <>
    hidden <>
    command "date" (info (pure $ addBlock dateBlock) (progDesc "Load the date and time block.")) <>
    command "cpu" (info (pure $ addBlock $ cpuUsageBlock 1) (progDesc "Load the cpu usage block.")) <>
    command "battery" (info (pure $ addBlock $ batteryBlock) (progDesc "Load the battery block.")) <>
    command "networkmanager" (info (pure $ addBlock networkManagerBlock) (progDesc "Load the network-manager block.")) <>
    command "script" (info scriptBlockParser (progDesc "Display the output of an external script as a block."))
  )

scriptBlockParser :: Parser (BarIO ())
scriptBlockParser = helper <*> do
  poll <- switch $ long "poll" <> short 'p' <> help "Run script in poll mode (every line of output updates the block)."
  script <- strArgument (metavar "SCRIPT" <> help "The script that will be executed with a shell.")
  return $ (if poll then addBlock . pollScriptBlock else addBlock . scriptBlock) script
