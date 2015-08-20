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
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH

type EmailAddress = Text
type Token = ByteString

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
TokenRow sql=tokens
  token ByteString
  read Bool
  create Bool
  edit Bool
  userId UserId
  reminderId ReminderId Maybe
  UniqueToken token
  deriving Show
Reminder sql=reminders
  body Text
  sendAt UTCTime
  sent Bool
  userId UserId
  deriving Show
|]
