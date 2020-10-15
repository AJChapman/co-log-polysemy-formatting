{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module      : Colog.Polysemy.Formatting.WithLog
Description : A constraint for adding the logging effect.
-}
module Colog.Polysemy.Formatting.WithLog
  ( WithLog'
  , WithLog
  ) where

import Colog (Msg(..), Severity(..))
import Colog.Polysemy (Log(..))
import GHC.Stack (HasCallStack)
import Polysemy

-- | This constraint allows you to specify a custom message type.
-- Otherwise, use 'WithLog' instead.
type WithLog' msg r = (HasCallStack, Member (Log msg) r)

-- | Add this constraint to a type signature to require
-- the 'Log' effect, with callstack support, using the 'Msg Severity' message type.
type WithLog r = WithLog' (Msg Severity) r


