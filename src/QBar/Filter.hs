{-# LANGUAGE TemplateHaskell #-}

module QBar.Filter where

import QBar.BlockOutput
import QBar.BlockText

import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Aeson.TH
import Data.Colour.RGBSpace.HSV (hsv)
import qualified Data.Text.Lazy as T
import Control.Lens hiding (index)

import Numeric (showHex)
import Data.Colour.RGBSpace


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

applyFilter :: Filter -> Double -> [BlockOutput] -> [BlockOutput]
applyFilter (StaticFilter None) = static id
applyFilter (AnimatedFilter Rainbow) = rainbow

static :: a -> Double -> a
static fn _ = fn


coloredText :: T.Text -> T.Text -> T.Text
coloredText color text = "<span color='" <> color <> "'>" <> text <> "</span>"


pangoColor :: RGB Double -> T.Text
pangoColor (RGB r g b) =
  let r' = hexColorComponent r
      g' = hexColorComponent g
      b' = hexColorComponent b
  in "#" <> r' <> g' <> b'
  where
    hexColorComponent :: Double -> T.Text
    hexColorComponent val = paddedHexComponent $ T.pack $ showHex (max 0 $ min 255 (truncate (val * 255) :: Int)) ""
    paddedHexComponent hex =
      let len = 2 - T.length hex
          padding = if len == 1 then "0" else ""
      in padding <> hex


rainbow :: Double -> [BlockOutput] -> [BlockOutput]
rainbow time blocks = reverse $ evalState (mapM rainbowBlock $ reverse blocks) 0
  where
    rainbowBlock :: BlockOutput -> State Integer BlockOutput
    rainbowBlock block = do
      let text = removePango $ block^.fullText
      let chars = T.unpack . T.reverse $ text
      coloredChars <- mapM rainbowChar chars
      let rainbowText = T.concat . reverse $ coloredChars
      return $ fullText .~ pangoText rainbowText $ block
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
