{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Dismissive.Internal.Types where

import BasePrelude hiding (Unique)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH

type EmailAddress = Text

share [mkPersist sqlSettings] [persistLowerCase|
User sql=users
  name Text
  email EmailAddress
  UniqueEmail email
  UniqueName name
  deriving Show
Code sql=codes
  code Text
  generatedAt UTCTime
  valid Bool
  userId UserId
  UniqueCode code
  deriving Show
Token sql=tokens
  token Text
  read Bool
  append Bool
  userId UserId
  UniqueToken token
  deriving Show
Reminder sql=reminders
  subject Text Maybe
  body Text
  sendAt UTCTime
  sent Bool
  userId UserId
  deriving Show
|]
