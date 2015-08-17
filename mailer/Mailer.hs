{-# LANGUAGE FlexibleContexts #-}

module Mailer where

import BasePrelude hiding (left)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Dismissive.Types (EmailAddress)
import Network.Wreq

data Mailer =
  Mailer { mailerKey :: Text
         , mailerDomain :: Text
         }

data Email =
  Email { emailTo :: EmailAddress
        , emailFromName :: Text
        , emailFrom :: EmailAddress
        , emailReplyTo :: EmailAddress
        , emailSubject :: Text
        , emailBody :: Text
        }

type EmailAddressLocal = Text

data LocalEmail =
  LocalEmail { localEmailTo :: EmailAddress
             , localEmailFromName :: Text
             , localEmailFrom :: EmailAddressLocal
             , localEmailReplyTo :: EmailAddressLocal
             , localEmailSubject :: Text
             , localEmailBody :: Text
             }

fullEmail :: Text -> EmailAddressLocal -> EmailAddress
fullEmail domain local = local <> "@" <> domain

globalize :: Text -> LocalEmail -> Email
globalize domain LocalEmail {..} =
  Email { emailTo = localEmailTo
        , emailFromName = localEmailFromName
        , emailFrom = fullEmail domain localEmailFrom
        , emailReplyTo = fullEmail domain localEmailReplyTo
        , emailSubject = localEmailSubject
        , emailBody = localEmailBody
        }

instance ToJSON Email where
  toJSON Email{..} =
    object [ "auto_html" .= False
           , "to" .= [object ["email" .= emailTo]]
           , "from_name" .= emailFromName
           , "from_email" .= emailFrom
           , "headers" .= object ["Reply-To" .= emailReplyTo]
           , "subject" .= emailSubject
           , "text" .= emailBody
           ]

data MandrillError = InvalidKey
                   | PaymentRequired
                   | UnknownSubaccount
                   | ValidationError
                   | GeneralError
                   | UnknownError Text
                   | UnknownEvent Text
                     deriving (Show, Eq)

data SendError = Rejected Text
               | MandrillError MandrillError
               | MalformedResponse
               | UnrecognizedStatusCode Int
                 deriving Show

data MResponse = MSuccess Text
               | MRejected Text
               | MError MandrillError

instance FromJSON MResponse where
  parseJSON (Object v) = v .: "status" >>= \case
    "rejected" -> MRejected <$> v .: "reject_reason"
    "sent" -> MSuccess <$> v .: "_id"
    "error" -> v .: "name" <&> \case
      "Invalid_Key" -> MError InvalidKey
      "PaymentRequired" -> MError PaymentRequired
      "UnknownSubaccount" -> MError UnknownSubaccount
      "ValidationError" -> MError ValidationError
      "GeneralError" -> MError GeneralError
      text -> MError (UnknownError text)
    e -> return $ MError (UnknownEvent e)
  parseJSON _ = mempty

responseJSON :: FromJSON a => Getter (Response ByteString) (Maybe a)
responseJSON = responseBody . to decode

ensure :: Monad m => Bool -> a -> EitherT a m ()
ensure True  _   = return ()
ensure False err = left err

mandrill :: (MonadIO m, MonadReader Mailer m) => Email -> m (Response ByteString)
mandrill email = do
  key <- asks mailerKey
  let body = object ["message" .= email, "key" .= key]
  liftIO $ post "https://mandrillapp.com/api/1.0/messages/send" (toJSON body)

sendMail :: MonadIO m => LocalEmail -> ReaderT Mailer m (Either SendError ())
sendMail localEmail = runEitherT $ do
  domain <- asks mailerDomain
  res <- mandrill (globalize domain localEmail)
  let status = res ^. responseStatus.statusCode
  ensure (status == 200) (UnrecognizedStatusCode status)
  let maybeResponses = res ^. responseJSON
  case maybeResponses of
    Just [MSuccess _]  -> return ()
    Just [MRejected r] -> left (Rejected r)
    Just [MError e]    -> left (MandrillError e)
    _                  -> left MalformedResponse
