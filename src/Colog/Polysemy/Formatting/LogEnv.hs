{-|
Module      : Colog.Polysemy.Formatting.LogEnv
Description : The 'LogEnv' datatype, to describe the logging environment.

|-}
module Colog.Polysemy.Formatting.LogEnv
  ( LogEnv(..)
  , newLogEnv
  ) where

import Data.Time (TimeZone, getCurrentTimeZone)
import System.IO (Handle)

import Colog.Polysemy.Formatting.Color (UseColor, termColorSupport)

-- | A datatype to contain the logging environment: whether to use color output (if supported by the terminal), and the timezone to stamp messages in.
data LogEnv = LogEnv UseColor TimeZone

-- | Create a 'LogEnv' suitable for the given handle.
-- If the output is an interactive terminal which supports color, then the output will be in color.
-- If not then the output will be plain text without color.
-- The timezone used will be that of the current machine.
newLogEnv :: Handle -> IO LogEnv
newLogEnv h = do
  zone <- getCurrentTimeZone
  useColor <- termColorSupport h
  pure $ LogEnv useColor zone
