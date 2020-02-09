{-# LANGUAGE ApplicativeDo #-}

module QBar.Cli where

import QBar.Theme

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Options.Applicative

data BarCommand = BarServerCommand | SetThemeCommand Text

barCommandParser :: Parser BarCommand
barCommandParser = hsubparser (
    command "server" (info (pure BarServerCommand) (progDesc "Start a new qbar server. Should be called by swaybar, i3bar or or another i3bar-protocol compatible host process.")) <>
    command "theme" (info themeCommandParser (progDesc "Change the theme of the running qbar server.")) <>
    command "default" (info (pure $ SetThemeCommand "default") (progDesc "Shortcut for 'qbar theme default'.")) <>
    command "rainbow" (info (pure $ SetThemeCommand "rainbow") (progDesc "Shortcut for 'qbar theme rainbow'."))
  )

themeCommandParser :: Parser BarCommand
themeCommandParser = SetThemeCommand <$> strArgument (metavar "THEME" <> completeWith (map TL.unpack themeNames))

data MainOptions = MainOptions {
  verbose :: Bool,
  indicator :: Bool,
  socketLocation :: Maybe T.Text,
  barCommand :: BarCommand
}

mainOptionsParser :: Parser MainOptions
mainOptionsParser = do
  verbose <- switch $ long "verbose" <> short 'v' <> help "Print more diagnostic output to stderr (including a copy of every bar update)."
  indicator <- switch $ long "indicator" <> short 'i' <> help "Show render indicator."
  socketLocation <- optional $ strOption $ long "socket" <> short 's' <> metavar "SOCKET" <> help "Control socket location. By default determined by WAYLAND_SOCKET location."
  barCommand <- barCommandParser
  return MainOptions {verbose, indicator, socketLocation, barCommand}

parser :: ParserInfo MainOptions
parser = info (mainOptionsParser <**> helper)
  (fullDesc <> header "qbar - queezles {i3,sway}bar infrastructure")

parserPrefs :: ParserPrefs
parserPrefs = prefs showHelpOnEmpty

parseOptions :: IO MainOptions
parseOptions = customExecParser parserPrefs parser