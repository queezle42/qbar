{-# LANGUAGE OverloadedStrings #-}

module QBar.Pango (Pango, parsePango, removeFormatting) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Data.Text.Lazy as T
import Data.Attoparsec.Text.Lazy as A

data Pango = PText T.Text
  | PTag T.Text [(T.Text, T.Text)] Pango
  | PList [Pango]
  deriving Show

pList :: [Pango] -> Pango
pList [one] = one
pList more = PList more

pangoParser :: Parser Pango
pangoParser = pList <$> many' (choice [normalTextParser, tagParser])
  where
    normalTextParser :: Parser Pango
    normalTextParser  = PText . T.fromStrict <$> A.takeWhile1 (notInClass "<>")

    tagParser :: Parser Pango
    tagParser = do
      tagName <- char '<' >> identifier
      attributes <- many' $ do
        space >> skipSpace
        attributeName <- identifier
        void $ char '='
        value <- char '\'' *> many' (notChar '\'') <* char '\''
          <|> char '"' *> many' (notChar '"') <* char '"'
        return (attributeName, T.pack value)
      void $ char '>'
      content <- pangoParser
      -- close tag
      void $ string $ T.toStrict $ "</" <> tagName <> ">"
      return $ PTag tagName attributes content

    identifier :: Parser T.Text
    identifier = T.pack <$> many1 (letter <|> digit)

parsePango :: T.Text -> Either String Pango
parsePango text = parseOnly (pangoParser <* endOfInput) (T.toStrict text)

removeFormatting :: Pango -> T.Text
removeFormatting (PText text) = text
removeFormatting (PTag _ _ child) = removeFormatting child
removeFormatting (PList list) = mconcat $ map removeFormatting list