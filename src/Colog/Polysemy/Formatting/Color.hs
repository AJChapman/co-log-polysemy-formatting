{-# LANGUAGE LambdaCase #-}
{-|
Module      : Colog.Polysemy.Formatting.Color
Description : Terminal colour support for log messages.
|-}
module Colog.Polysemy.Formatting.Color
  ( Color(..)
  , UseColor(..)
  , termColorSupport
  , getWithFG
  ) where

import qualified Data.Text.Lazy.Builder as TLB
import System.Console.ANSI
       ( Color(..)
       , ColorIntensity(Vivid)
       , ConsoleLayer(Foreground)
       , SGR(..)
       , hSupportsANSIColor
       , setSGRCode
       )
import System.IO (Handle)
import Formatting

-- | A 'Bool'-isomorphic type for expressing whether output should use color or not.
data UseColor = DoUseColor | DontUseColor

-- | Detect whether the terminal at the given handle (e.g. @stdout@) supports color output.
termColorSupport :: Handle -> IO UseColor
termColorSupport h =
  hSupportsANSIColor h >>= \case
    True -> pure DoUseColor
    False -> pure DontUseColor

-- | Generate a function for setting the foreground color.
getWithFG
  :: UseColor -- ^ Whether to actually use color, or ignore any given color.
  -> Color -- ^ The color to set the foreground text to
  -> TLB.Builder -> TLB.Builder
getWithFG DontUseColor _ txt = txt
getWithFG DoUseColor color txt =
  bformat (string % builder % string)
    (setSGRCode [SetColor Foreground Vivid color])
    txt
    (setSGRCode [Reset])
