-- Required for formatting
{-# LANGUAGE OverloadedStrings #-}

-- Required for Polysemy
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- Required for co-log-polysemy-formatting.
-- This should re-export everything you need for logging.
import Colog.Polysemy.Formatting

-- Other imports for this example
import Data.Function ((&))
import Formatting
import Polysemy
import Polysemy.Async
import System.IO (stderr)

-- main needs the 'HasCallStack' constraint for log functions to know where they were called from
main :: HasCallStack => IO ()
main = do
  -- Set up a logging environment, logging to stderr and using the local timezone
  logEnvStderr <- newLogEnv stderr

  (do
    -- This debug message will show up only if 'debugMode' is True
    logDebug "MyApp version 0.1.0.0"

    -- Run our Polysemy program
    program
    )
      -- Set the level of logging we want (for more control see 'filterLogs')
      & setLogLevel Debug

      -- This lets us log the thread id and message timestamp with each log message
      & addThreadAndTimeToLog

      -- If you are using the 'Async' effect then interpret it here, after adding the thread and time,
      -- but before running the log action.
      & asyncToIO

      -- Log to stderr, using our logging environment
      & runLogAction (logTextStderr & cmap (renderThreadTimeMessage logEnvStderr))

      & runM

program :: (WithLog r, Members '[Async, Embed IO] r) => Sem r ()
program = do
  -- This concurrency is just here to demonstrate that it is possible.
  -- It isn't required.
  sequenceConcurrently $
    replicate 10 asyncProg
    <> [logError ("Error message: '" % accessed fst text <> "', number: " % accessed snd int) ("It's all broken!", 17)]
    <> replicate 10 asyncProg
  pure ()
  where
    asyncProg = do
      logInfo "Hello, logging!"
      embed $ fprintLn "Hello, logging!"
