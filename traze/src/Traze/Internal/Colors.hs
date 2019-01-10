module Traze.Internal.Colors (
  trazeColorStrings
) where

import Data.Fixed

import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSV

goldenRatio :: Double
goldenRatio = 0.618033988749895

nextHue :: Double -> Double
nextHue h = (h + goldenRatio) `mod'` 1

getTrazeColor :: Double -> Colour Double
getTrazeColor h = fixFuckingColour $ hsv (h * 360)  0.5 0.95

-- why the fuck doesn't colour come with this?
fixFuckingColour :: RGB Double -> Colour Double
fixFuckingColour rgb = sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)

trazeColors :: [Colour Double]
trazeColors = map getTrazeColor $ iterate nextHue 0.333333 

trazeColorStrings :: [String]
trazeColorStrings = map sRGB24show trazeColors
