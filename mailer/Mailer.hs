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

data Mailer = Mailer { mailerKey :: Text }

newMailer :: Text -> Mailer
newMailer key = Mailer { mailerKey = key }

data Email = Email { emailTo :: EmailAddress
                   , emailFromName :: Text
                   , emailFromEmail :: EmailAddress
                   , emailReplyTo :: EmailAddress
                   , emailSubject :: Text
                   , emailBody :: Text
                   }

instance ToJSON Email where
  toJSON Email{..} = object [ "auto_html" .= False
                            , "to" .= [object ["email" .= emailTo]]
                            , "from_name" .= emailFromName
                            , "from_email" .= emailFromEmail
                            , "headers" .= object ["Reply-To" .= emailReplyTo]
                            , "subject" .= emailSubject
                            , "text" .= emailBody
                            ]

mandrill :: MonadIO m => Email -> ReaderT Mailer m (Response ByteString)
mandrill email = do
  key <- asks mailerKey
  let body = object ["message" .= email, "key" .= key]
  liftIO $ post "https://mandrillapp.com/api/1.0/messages/send" (toJSON body)

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

sendMail :: MonadIO m => Email -> ReaderT Mailer (EitherT SendError m) ()
sendMail email = do
  res <- mandrill email
  let status = res ^. responseStatus.statusCode
  lift $ ensure (status == 200) (UnrecognizedStatusCode status)
  let maybeResponses = res ^. responseJSON
  case maybeResponses of
    Just [MSuccess _]  -> return ()
    Just [MRejected r] -> lift $ left (Rejected r)
    Just [MError e]    -> lift $ left (MandrillError e)
    _ -> lift $ left MalformedResponse
