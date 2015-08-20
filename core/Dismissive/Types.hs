module Dismissive.Types (
  User(..),
  Code(..),
  Token,
  Reminder(..),
  EmailAddress,
  Permission(..)
) where

import BasePrelude
import Dismissive.Internal.Types

data Permission = PermissionReadAll
                | PermissionEditAll
                | PermissionCreate
                | PermissionEdit ReminderId
                  deriving (Eq, Show)
