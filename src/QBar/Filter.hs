{-# LANGUAGE TemplateHaskell #-}

module QBar.Filter where

import QBar.Core

import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Aeson.TH
import Data.Colour.RGBSpace.HSV (hsv)
import qualified Data.Text.Lazy as T

data Filter = StaticFilter StaticFilter
  | AnimatedFilter AnimatedFilter
  deriving Show

data StaticFilter = None
  deriving Show

data AnimatedFilter = Rainbow
  deriving Show

$(deriveJSON defaultOptions ''Filter)
$(deriveJSON defaultOptions ''StaticFilter)
$(deriveJSON defaultOptions ''AnimatedFilter)

isAnimatedFilter :: Filter -> Bool
isAnimatedFilter (AnimatedFilter _) = True
isAnimatedFilter _ = False

applyFilter :: Filter -> Double -> [Block] -> [Block]
applyFilter (StaticFilter None) = static id
applyFilter (AnimatedFilter Rainbow) = rainbow

static :: a -> Double -> a
static fn _ = fn

rainbow :: Double -> [Block] -> [Block]
rainbow time blocks = reverse $ evalState (mapM rainbowBlock $ reverse blocks) 0
  where
    rainbowBlock :: Block -> State Integer Block
    rainbowBlock block = do
      let cleanBlock = removePango block
      let text = getFullText cleanBlock
      let chars = T.unpack . T.reverse $ text
      coloredChars <- mapM rainbowChar chars
      let rainbowText = T.concat . reverse $ coloredChars
      return $ pangoMarkup $ fullText rainbowText $ cleanBlock
    rainbowChar :: Char -> State Integer T.Text
    rainbowChar char = do
      color <- nextRainbowColor
      return $ coloredText color $ T.singleton char
    nextRainbowColor :: State Integer T.Text
    -- nextRainbowColor = state $ \index -> (rainbowColor (fromInteger index), index + 1)
    nextRainbowColor = do
      index <- get
      put $ index + 1
      return $ rainbowColor (fromInteger index + time * 10)

rainbowColor :: Double -> T.Text
rainbowColor position =
  let hue' = position * 3
      color = hsv hue' 0.8 1.0
  in pangoColor color
