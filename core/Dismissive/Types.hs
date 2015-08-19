module Dismissive.Types (
  User(..),
  Code(..),
  Reminder(..),
  EmailAddress,
  Permission(..)
) where

import BasePrelude
import Dismissive.Internal.Types

data Permission = PermissionReadAll
                | PermissionEditAll
                | PermissionCreate
                | PermissionEditReminder ReminderId
                  deriving (Eq, Show)
