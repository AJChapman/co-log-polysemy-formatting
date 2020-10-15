{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Colog.Polysemy.Formatting.Render
Description : Render log messages.
-}
module Colog.Polysemy.Formatting.Render
  ( renderThreadTimeMessage
  , renderThreadTimeMessageShort
  , fIso8601Tz
  , fSeverity
  , fThread
  , fCallerFromStack
  , fCaller
  , fCallerLong'
  , fCallerLong
  , fCallerShort'
  , fCallerShort
  ) where

import Colog (Msg(..), Severity(..))
import Control.Concurrent (ThreadId)
import Data.Char (isUpper)
import Data.Function ((&))
import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder)
import Data.Time (FormatTime, utcToZonedTime)
import Formatting
import Formatting.Time
import GHC.Stack (CallStack, SrcLoc(..), getCallStack)

import Colog.Polysemy.Formatting.Color (Color(..), UseColor, getWithFG)
import Colog.Polysemy.Formatting.LogEnv (LogEnv(..))
import Colog.Polysemy.Formatting.ThreadTimeMessage (ThreadTimeMessage(..))

-- | Render the message, optionally in color, with green " | " separating fields, and these fields:
--
--     * Severity (e.g. "INFO", see 'fSeverity'),
--     * Timestamp (e.g. "2020-10-13T16:58:43.982720690+1100", see 'fIso8601Tz'),
--     * Thread Id (e.g. "Thread     8", see 'fThread'),
--     * Caller (e.g. "MyApp.CLI.cliMain#43", see 'fCallerLong'), and
--     * The log message itself.
--
-- E.g: @"INFO | 2020-10-13T17:06:52.408921221+1100 | Thread     8 | MyApp.CLI.cliMain#43 | MyApp version 0.1.0.0"@
--
-- The first three columns are fixed-width, which makes visual scanning of the log easier.
renderThreadTimeMessage :: LogEnv -> ThreadTimeMessage -> T.Text
renderThreadTimeMessage = renderThreadTimeMessage' fCallerLong

-- | Like 'renderThreadTimeMessage', but abbreviate the caller by removing lowercase letters from the module name.
renderThreadTimeMessageShort :: LogEnv -> ThreadTimeMessage -> T.Text
renderThreadTimeMessageShort = renderThreadTimeMessage' fCallerShort

renderThreadTimeMessage' :: ((Color -> Builder -> Builder) -> Format Builder (CallStack -> Builder)) -> LogEnv -> ThreadTimeMessage -> T.Text
renderThreadTimeMessage' renderCaller (LogEnv useColor zone) (ThreadTimeMessage threadId time (Msg severity stack message)) =
  let withFG = getWithFG useColor
  in sformat (fFieldsGreenBarSep useColor)
    [ bformat (fSeverity withFG) severity
    , bformat (fIso8601Tz withFG) (utcToZonedTime zone time)
    , bformat fThread threadId
    , bformat (renderCaller withFG) stack
    , bformat stext message
    ]

fFieldsGreenBarSep :: UseColor -> Format r ([Builder] -> r)
fFieldsGreenBarSep useColor = later $ \fields ->
  let withFG = getWithFG useColor
      sep = format builder $ withFG Green " | "
  in bformat (intercalated sep builder) fields

-- | Render a timestamp in ISO-8601 format, in color, to 9 decimal places,
-- e.g.: "2020-10-13T16:58:43.982720690+1100"
--
-- The "T" is rendered in green, the time in yellow, the rest without color.
fIso8601Tz :: FormatTime a => (Color -> Builder -> Builder) -> Format r (a -> r)
fIso8601Tz withFG = later $ \time -> mconcat
  [ bformat dateDash time
  , withFG Green "T"
  , withFG Yellow $ bformat hmsL time
  , withFG Yellow $ bformat (right 10 '0') (bformat decimals time)
  , bformat tz time
  ]

-- | Render the 'Severity' of the message, with color, using 4 characters to maintain alignment:
--
--     * DBUG in green,
--     * INFO in blue,
--     * WARN in yellow, or
--     * ERR in red.
fSeverity :: (Color -> Builder -> Builder) -> Format r (Severity -> r)
fSeverity withFG = later $ \case
  Debug   -> withFG Green  "DBUG"
  Info    -> withFG Blue   "INFO"
  Warning -> withFG Yellow "WARN"
  Error   -> withFG Red    "ERR "

-- | Render the Id of the thread that the log message was generated in,
-- with a fixed width, at least until the thread Ids exceed 100,000,
-- e.g. "Thread    97".
fThread :: Format r (ThreadId -> r)
fThread = later $ \tid ->
  let s = show tid
  in bformat ("Thread " % left 5 ' ') (drop 9 s)

fCallerFromStack :: Format r (Maybe (String, SrcLoc) -> r) -> Format r (CallStack -> r)
fCallerFromStack = mapf callStackLoc
  where
    callStackLoc :: CallStack -> Maybe (String, SrcLoc)
    callStackLoc cs =
      case getCallStack cs of
        []                             -> Nothing
        [(name, loc)]                  -> Just (name, loc)
        (_, loc) : (callerName, _) : _ -> Just (callerName, loc)

fCaller :: (Color -> Builder -> Builder) -> Format r (String -> String -> Int -> r)
fCaller withFG =
  string % "." % colored Cyan string % "#" % colored Red int
  where
    colored c f = later $ \input ->
      bformat f input & withFG c

fCallerLong' :: (Color -> Builder -> Builder) -> Format r (Maybe (String, SrcLoc) -> r)
fCallerLong' withFG = maybed "<unknown loc>" $
  later $ \(name, SrcLoc{..}) ->
    bformat (fCaller withFG)
      srcLocModule
      name
      srcLocStartLine

-- | Render the fully qualified function that called the log function,
-- and line number in the source file, e.g. "MyApp.CLI.cliMain#43",
-- with the function name in cyan and line number in red.
fCallerLong :: (Color -> Builder -> Builder) -> Format r (CallStack -> r)
fCallerLong withFG = fCallerFromStack (fCallerLong' withFG)

fCallerShort' :: (Color -> Builder -> Builder) -> Format r (Maybe (String, SrcLoc) -> r)
fCallerShort' withFG = maybed "?" $
  later $ \(name, SrcLoc{..}) ->
    bformat (fCaller withFG)
      (abbreviateModule srcLocModule)
      name
      srcLocStartLine
  where
    abbreviateModule =
      filter (\c -> isUpper c || c == '.')

-- | Render the fully qualified function that called the log function,
-- and line number in the source file, abbreviating the module path by
-- removing lower-case letters, e.g. "MA.CLI.cliMain#43",
-- with the function name in cyan and line number in red.
fCallerShort :: (Color -> Builder -> Builder) -> Format r (CallStack -> r)
fCallerShort withFG = fCallerFromStack (fCallerShort' withFG)
