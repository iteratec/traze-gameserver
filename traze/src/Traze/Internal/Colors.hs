{-|
Module      : Colors
Description : color generation
Copyright   : (c) Benjamin Brunzel, 2018
License     : BSD3
Maintainer  : benjamin.brunzel@gmail.com

This module provides functions for procedurally 
generating distingushable colors to be used for
traze bikes on the grid. 

It uses the [HSV Color Format](https://en.wikipedia.org/wiki/HSL_and_HSV)
and generates colors based on hue manipulation
using the golden ratio.
-}

module Traze.Internal.Colors (
    trazeColorStrings
  , goldenRatio
  , trazeColors
  , nextHue
  , getTrazeColor
  , rgbToColour
) where


import Data.Fixed

import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSV

-- | approximation of the golden ratio constant
goldenRatio :: Double
goldenRatio = 0.618033988749895

-- | infinite list of color strings to choose from for traze bikes
trazeColorStrings :: [String]
trazeColorStrings = map sRGB24show trazeColors

-- | infinite list of traze colors
trazeColors :: [Colour Double]
trazeColors = map getTrazeColor $ iterate nextHue 0.333333 

-- | return the next Hue based on the golden ratio
nextHue :: Double -> Double
nextHue h = (h + goldenRatio) `mod'` 1

-- | create a traze color from a given hue value
getTrazeColor :: Double         -- ^ hue value as a value between 0 and 1
              -> Colour Double  -- ^ resulting traze color
getTrazeColor h = rgbToColour $ hsv (h * 360)  0.5 0.95

-- | convert an RGB color to the Colour type
rgbToColour :: RGB Double -> Colour Double
rgbToColour rgb = sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)

