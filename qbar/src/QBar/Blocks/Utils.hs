module QBar.Blocks.Utils (
  ensure,
  formatFloatN,
  parseFile,
  tryMaybe',
  tryMaybe,
) where

import QBar.Prelude

import Control.Exception (SomeException, catch)
import Data.Attoparsec.Text.Lazy qualified as AT
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO
import Numeric (showFFloat)

formatFloatN :: RealFloat a => Int -> a -> T.Text
formatFloatN = formatFloatN' 0

formatFloatN' :: RealFloat a => Int -> Int -> a -> T.Text
formatFloatN' padWithZeros decimalPlaces f = T.justifyRight (fromIntegral padWithZeros) '0' . T.pack $ showFFloat (Just decimalPlaces) f ""

ensure :: (a -> Bool) -> a -> Maybe a
ensure f a
  | f a = Just a
  | otherwise = Nothing

tryMaybe :: MonadIO m => IO a -> m (Maybe a)
tryMaybe a = tryMaybe' (Just <$> a)

tryMaybe' :: MonadIO m => IO (Maybe a) -> m (Maybe a)
tryMaybe' a = liftIO . catch a $ \(_ :: SomeException) -> return Nothing

parseFile :: MonadIO m => FilePath -> AT.Parser a -> m (Maybe a)
parseFile path parser = tryMaybe' $ do
  fileContents <- TIO.readFile path
  return . AT.maybeResult $ AT.parse parser fileContents
