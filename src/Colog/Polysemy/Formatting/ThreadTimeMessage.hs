{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Colog.Polysemy.Formatting.ThreadTimeMessage
Description : A message type that includes ThreadId and a timestamp, wrapping a 'Message'.
|-}
module Colog.Polysemy.Formatting.ThreadTimeMessage
  ( ThreadTimeMessage(..)
  , HasSeverity(..)
  , ttmSeverity
  , addThreadAndTimeToLog
  ) where

import Prelude hiding (log)

import Colog (Message, Msg(..), Severity(..))
import Colog.Polysemy (Log(..), log)
import Control.Concurrent (ThreadId, myThreadId)
import Data.Time (UTCTime, getCurrentTime)
import Polysemy

-- | A log message which wraps a 'Message', adding a 'ThreadId' and 'UTCTime' timestamp.
data ThreadTimeMessage = ThreadTimeMessage
  { ttmThreadId :: ThreadId
  , ttmTime     :: UTCTime
  , ttmMsg      :: Message
  }

-- | Get the severity of the message.
ttmSeverity :: ThreadTimeMessage -> Severity
ttmSeverity = msgSeverity . ttmMsg

class HasSeverity msg where
  getSeverity :: msg -> Severity

instance HasSeverity (Msg Severity) where
  getSeverity = msgSeverity

instance HasSeverity ThreadTimeMessage where
  getSeverity = ttmSeverity

-- | Add the thread id and a timestamp to messages in the log.
-- This should be called /before/ any use of 'Polysemy.Async.asyncToIO', otherwise all log messages will have the same thread id.
-- It is best called /after/ any use of 'Colog.Polysemy.Formatting.filterLogs', otherwise you're needlessly processing messages that will never be logged (TODO: test this assertion is true).
addThreadAndTimeToLog
  :: Members
    '[ Embed IO
     , Log ThreadTimeMessage
     ] r
  => Sem (Log Message ': r) a
  -> Sem r a
addThreadAndTimeToLog = interpret $ \case
  Log msg -> do
    threadId <- embed myThreadId
    time <- embed getCurrentTime
    log $ ThreadTimeMessage threadId time msg


