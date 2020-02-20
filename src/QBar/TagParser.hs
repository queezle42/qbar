module QBar.TagParser where

import QBar.BlockOutput

import Control.Monad (void)
import Data.Functor (($>))
import Data.Either (either)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import Data.Attoparsec.Text.Lazy as A

type TagState = (Bool, Importance)

tagParser :: Parser BlockText
tagParser = parser (False, normalImportant)
  where
    parser :: TagState -> Parser BlockText
    parser (active, importance) = mconcat <$> many' singleElementParser
      where
        singleElementParser :: Parser BlockText
        singleElementParser = choice [textParser, activeTagParser, importanceTagParser]

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


parseTags :: T.Text -> Either String BlockText
parseTags text = parseOnly (tagParser <* endOfInput) (T.toStrict text)

parseTags' :: T.Text -> BlockOutput
parseTags' = either (mkErrorOutput . T.pack) mkBlockOutput . parseTags

parseTags'' :: T.Text -> T.Text -> BlockOutput
parseTags'' full short = either (mkErrorOutput . T.pack) id $ do
  full' <- parseTags $ full
  short' <- parseTags $ short
  return $ mkBlockOutput' full' short'
