{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Dismissive.Types where

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
  idUser UserId
  UniqueCode code
  deriving Show
Token sql=tokens
  token Text
  idUser UserId
  UniqueToken token
  deriving Show
Message sql=messages
  subject Text Maybe
  body Text
  send_at UTCTime
  sent Bool
  idUser UserId
  deriving Show
|]
