{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Colog.Polysemy.Formatting
-- Description : A Polysemy effect for logging using Co-Log.
--
-- Checklist for use:
--
--     1. Add `co-log-polysemy-formatting` to your `build-depends` in your .cabal file,
--     2. Turn on the OverloadedStrings language extension,
--     3. `import Colog.Polysemy.Formatting`
--     4. (optional) Add the 'HasCallStack' constrain to your `main` if it calls any logging functions directly,
--     5. Create a logging environment with 'newLogEnv', e.g. like this: @logEnvStderr <- newLogEnv stderr@
--     6. To create log messages from within the 'Sem' monad, add the @'WithLog' r@ constraint and then call any of the logging functions: 'logDebug', 'logInfo', 'logWarning', 'logError', or 'logException'.
--        Note that these take a "Formatting" formatter, not a String/Text/etc.
--        But note also that they can still take a string literal, which will be transformed into a formatter using OverloadedStrings.
--     7. (optional) When interpreting your program, add a call to 'filterLogs' to e.g. filter out Debug messages for a production build,
--     8. call 'addThreadAndTimeToLog',
--     9. call 'runLogAction', including a call to 'renderThreadTimeMessage' or 'Color.Polysemy.Formatting.Render.renderThreadTimeMessageShort' with the LogEnv you created in step 4, e.g. like this: @runLogAction (logTextStderr & cmap (renderThreadTimeMessage logEnvStderr))@.
--
-- Example of usage (this is a copy of example/Main.hs, which you can compile and run for yourself):
--
-- > -- Required for formatting
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > -- Required for Polysemy
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE FlexibleContexts #-}
-- > {-# LANGUAGE GADTs #-}
-- >
-- > -- Required for co-log-polysemy-formatting.
-- > -- This should re-export everything you need for logging.
-- > import Colog.Polysemy.Formatting
-- >
-- > -- Other imports for this example
-- > import Data.Function ((&))
-- > import Formatting
-- > import Polysemy
-- > import Polysemy.Async
-- > import System.IO (stderr)
-- >
-- > -- main needs the 'HasCallStack' constraint for log functions to know where they were called from
-- > main :: HasCallStack => IO ()
-- > main = do
-- >   -- Set up a logging environment, logging to stderr and using the local timezone
-- >   logEnvStderr <- newLogEnv stderr
-- >
-- >   (do
-- >     -- This debug message will show up only if 'debugMode' is True
-- >     logDebug "MyApp version 0.1.0.0"
-- >
-- >     -- Run our Polysemy program
-- >     program
-- >     )
-- >       -- Set the level of logging we want (for more control see 'filterLogs')
-- >       & setLogLevel Debug
-- >
-- >       -- This lets us log the thread id and message timestamp with each log message
-- >       -- It transforms the 'Log Message' effect into a 'Log ThreadTimeMessage' effect.
-- >       & addThreadAndTimeToLog
-- >
-- >       -- If you are using the 'Async' effect then interpret it here, after adding the thread and time,
-- >       -- but before running the log action.
-- >       & asyncToIO
-- >
-- >       -- Log to stderr, using our logging environment
-- >       & runLogAction (logTextStderr & cmap (renderThreadTimeMessage logEnvStderr))
-- >
-- >       & runM
-- >
-- > -- The 'WithLog r' constraint expands to '(HasCallStack, Member (Log Message) r)'
-- > program :: (WithLog r, Members '[Async, Embed IO] r) => Sem r ()
-- > program = do
-- >   -- This concurrency is just here to demonstrate that it is possible.
-- >   -- It isn't required.
-- >   _ <- sequenceConcurrently $
-- >     replicate 10 asyncProg
-- >     <> [logError ("Error message: '" % accessed fst text <> "', number: " % accessed snd int) ("It's all broken!", 17 :: Int)]
-- >     <> replicate 10 asyncProg
-- >   pure ()
-- >   where
-- >     asyncProg = do
-- >       logInfo "Hello, logging!"
-- >       embed $ fprintLn "Hello, logging!"
--
-- The above produces this:
--
-- <<example/output.png example program output>>
module Colog.Polysemy.Formatting
  (
  -- * Creating log messages
    WithLog
  , WithLog'
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException

  -- * Interpreting the log
  , newLogEnv
  , ignoreLog
  , filterLogs
  , setLogLevel
  , addThreadAndTimeToLog
  , renderThreadTimeMessage
  , renderThreadTimeMessageShort

  -- * Re-exports from other packages
  , HasCallStack
  , runLogAction
  , logTextStdout
  , logTextStderr
  , logTextHandle
  , cmap
  , Severity(..)
  , Msg(..)
  ) where

import Prelude hiding (log)

import Colog (Msg(..), Severity(..), cmap)
import Colog.Actions (logTextHandle, logTextStderr, logTextStdout)
import Colog.Polysemy (Log(..), runLogAction)
import qualified Colog.Polysemy as Colog
import Control.Category ((>>>))
import Control.Exception (Exception, displayException)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Formatting
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Polysemy

import Colog.Polysemy.Formatting.LogEnv (newLogEnv)
import Colog.Polysemy.Formatting.Render (renderThreadTimeMessage, renderThreadTimeMessageShort)
import Colog.Polysemy.Formatting.ThreadTimeMessage (addThreadAndTimeToLog, HasSeverity(..))
import Colog.Polysemy.Formatting.WithLog (WithLog, WithLog')

-- | Log a message with an explicit severity.
-- You probably don't want to use this directly.
-- Use 'logInfo', 'logError', etc. instead.
log :: WithLog r => Severity -> Format (Sem r ()) a -> a
log sev m = withFrozenCallStack $
  runFormat m
    $ Colog.log
    . Msg sev callStack
    . TL.toStrict
    . TLB.toLazyText

-- | Log a debug message in the given format.
logDebug :: WithLog r => Format (Sem r ()) a -> a
logDebug = withFrozenCallStack $ log Debug

-- | Log an info message in the given format.
logInfo :: WithLog r => Format (Sem r ()) a -> a
logInfo = withFrozenCallStack $ log Info

-- | Log a warning in the given format.
logWarning :: WithLog r => Format (Sem r ()) a -> a
logWarning = withFrozenCallStack $ log Warning

-- | Log an error in the given format.
logError :: WithLog r => Format (Sem r ()) a -> a
logError = withFrozenCallStack $ log Error

-- | Log the exception as an error.
logException :: (WithLog r, Exception e) => e -> Sem r ()
logException = withFrozenCallStack . logError string . displayException

-- | Interpret the 'Log' effect by completely ignoring all log messages.
ignoreLog :: Sem (Log msg ': r) a -> Sem r a
ignoreLog = interpret $ \case
  Log _ -> pure ()

-- | Remove any log messages that don't pass the given predicate.
--
-- E.g: @filterLogs ((<) Info . msgSeverity)@ will remove any logs that are 'Debug' or 'Info' severity, leaving only 'Warning's and 'Error's.
filterLogs
  :: Member (Log msg) r
  => (msg -> Bool)
  -> Sem (Log msg ': r) a
  -> Sem r a
filterLogs f = interpret $ \case
  Log msg -> if f msg
    then Colog.log msg
    else pure ()

-- | Only show logs that are this log level or higher (lower according to the Ord instance for 'Severity').
--
-- E.g: @setLogLevel Debug@ will show all logs, whereas
-- @setLogLevel Warning@ will show only warnings and errors.
setLogLevel
  :: ( HasSeverity msg
     , Member (Log msg) r
     )
  => Severity
  -> Sem (Log msg ': r) a
  -> Sem r a
setLogLevel level = filterLogs (getSeverity >>> (<=) level)
