module QBar.TagParser where

import QBar.BlockOutput
import QBar.Color

import Control.Applicative ((<|>))
import Data.Attoparsec.Text.Lazy as A
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T

type TagState = (Bool, Importance)

tagParser :: Parser BlockText
tagParser = parser (False, normalImportant)
  where
    parser :: TagState -> Parser BlockText
    parser (active, importance) = mconcat <$> many' singleElementParser
      where
        singleElementParser :: Parser BlockText
        singleElementParser = choice [textParser, activeTagParser, importanceTagParser, spanParser]

        textParser :: Parser BlockText
        textParser = mkText active importance . T.fromStrict <$> A.takeWhile1 (notInClass "<>")

        activeTagParser :: Parser BlockText
        activeTagParser = string "<active>" *> parser (True, importance) <* string "</active>"

        importanceTagParser :: Parser BlockText
        importanceTagParser = do
          (tag, importance') <- char '<' *> importanceParser <* char '>'
          result <- parser (active, importance')
          void $ string $ "</" <> tag <> ">"
          return result

    importanceParser :: Parser (TS.Text, Importance)
    importanceParser = choice $ map mkParser importanceTags
      where
        mkParser :: (TS.Text, Importance) -> Parser (TS.Text, Importance)
        mkParser (tag, importance) = string tag $> (tag, importance)
        importanceTags :: [(TS.Text, Importance)]
        importanceTags = [
            ("normal", normalImportant),
            ("warning", warnImportant),
            ("error", errorImportant),
            ("critical", criticalImportant)
          ]

    spanParser :: Parser BlockText
    spanParser = do
      void $ string "<span"
      (colors, backgrounds) <- unzip <$> (many' $ colorAttribute <|> backgroundAttribute)
      let color = listToMaybe . catMaybes $ colors
      let background = listToMaybe . catMaybes $ backgrounds
      void $ char '>'
      content <- T.fromStrict <$> A.takeWhile1 (notInClass "<>")
      void $ string "</span>"
      return $ mkStyledText color background content
      where
        colorAttributeParser :: Text -> Parser Color
        colorAttributeParser attribute = do
          space >> skipSpace
          void $ string $ T.toStrict attribute
          skipSpace
          void $ char '='
          skipSpace
          char '\'' *> colorParser <* char '\'' <|>
            char '"' *> colorParser <* char '"'

        colorAttribute :: Parser (Maybe Color, Maybe Color)
        colorAttribute = do
          color <- colorAttributeParser "color"
          pure (Just color, Nothing)
        backgroundAttribute :: Parser (Maybe Color, Maybe Color)
        backgroundAttribute = do
          background <- colorAttributeParser "background"
          pure (Nothing, Just background)



parseTags :: T.Text -> Either String BlockText
parseTags text = parseOnly (tagParser <* endOfInput) (T.toStrict text)

parseTags' :: T.Text -> BlockOutput
parseTags' = either (mkErrorOutput . T.pack) mkBlockOutput . parseTags

parseTags'' :: T.Text -> T.Text -> BlockOutput
parseTags'' full short = either (mkErrorOutput . T.pack) id $ do
  full' <- parseTags $ full
  short' <- parseTags $ short
  return $ mkBlockOutput' full' short'
