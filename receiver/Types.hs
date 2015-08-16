{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Types (
  MandrillEvent(..),
  Message(..),
  EmailAddress
) where

import BasePrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Servant (FromFormUrlEncoded, fromFormUrlEncoded)

type EmailAddress = Text

data Message = Message { to :: EmailAddress
                       , from :: EmailAddress
                       , body :: Text
                       , subject :: Text
                       } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "email"
            <*> v .: "from_email"
            <*> v .: "text"
            <*> v .: "subject"
  parseJSON _ = mzero

data MandrillEvent = MandrillEvent { msg :: Message
                                   , stamp :: UTCTime
                                   , event :: Text
                                   } deriving (Generic, Show)

instance FromJSON MandrillEvent where
  parseJSON (Object v) =
    MandrillEvent <$> v .: "msg"
                  <*> fmap utcify (v .: "ts")
                  <*> v .: "event"
  parseJSON _ = mzero

instance FromFormUrlEncoded [MandrillEvent] where
  fromFormUrlEncoded [("mandrill_events", singleNonsenseString)] =
    eitherDecodeText singleNonsenseString
  fromFormUrlEncoded _ = Left "malformed payload"

utcify :: Int -> UTCTime
utcify = posixSecondsToUTCTime . fromIntegral

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . LazyBS.fromStrict . Text.encodeUtf8
