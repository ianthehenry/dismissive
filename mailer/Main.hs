import BasePrelude hiding (insert)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Dismissive.Api
import qualified Data.Configurator as Conf
import Mailer
import Dismissive.Types

emailForMessage :: Text -> (Entity Message, Entity User) -> Email
emailForMessage domain ( Entity { entityKey = messageId, entityVal = message }
                       , Entity { entityVal = User { userEmail = toEmail } }
                       ) =
  Email toEmail "Dismissive" fromEmail replyToEmail subject body
  where replyToEmail = mconcat ["snooze+", keyShow messageId, "@", domain]
        fromEmail = "reminder@" <> domain
        subject = fromMaybe "Reminder!" (messageSubject message)
        body = messageBody message

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "mailer.conf", Conf.Optional "shared.conf"]
  connStr <- Conf.require conf "conn"
  mandrillKey <- Conf.require conf "mandrill-key"
  domain <- Conf.require conf "mail-domain"
  let mailer = newMailer mandrillKey

  withDismissiveIO connStr $ do
    messages <- unsentMessages
    liftIO $ putStrLn $ mconcat ["sending ", show (length messages), " messages"]
    for_ messages $ \(message, user) -> do
      let email = emailForMessage domain (message, user)
      runEitherT $ runReaderT (sendMail email) mailer
      markSent (entityKey message)
