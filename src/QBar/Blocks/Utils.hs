{-# LANGUAGE ScopedTypeVariables #-}

module QBar.Blocks.Utils where

import Control.Exception (IOException, catch)
import qualified Data.Attoparsec.Text.Lazy as AT
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Numeric (showFFloat)

formatFloatN :: RealFloat a => Int -> a -> T.Text
formatFloatN = formatFloatN' 0

formatFloatN' :: RealFloat a => Int -> Int -> a -> T.Text
formatFloatN' padWithZeros decimalPlaces f = T.justifyRight (fromIntegral padWithZeros) '0' . T.pack $ showFFloat (Just decimalPlaces) f ""

tryMaybe :: IO a -> IO (Maybe a)
tryMaybe a = tryMaybe' (Just <$> a)

tryMaybe' :: IO (Maybe a) -> IO (Maybe a)
tryMaybe' a = catch a (\(_ :: IOException) -> return Nothing)

parseFile :: FilePath -> AT.Parser a -> IO (Maybe a)
parseFile path parser = tryMaybe' $ do
  fileContents <- TIO.readFile path
  return . AT.maybeResult $ AT.parse parser fileContents
