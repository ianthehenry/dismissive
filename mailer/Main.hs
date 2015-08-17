import BasePrelude hiding (insert)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Dismissive.Api
import qualified Data.Configurator as Conf
import Mailer
import Dismissive.Types

emailForMessage :: (Entity Message, Entity User) -> LocalEmail
emailForMessage ( Entity { entityKey = messageId, entityVal = message }
                , Entity { entityVal = User { userEmail = toEmail } }
                ) =
  LocalEmail toEmail "Dismissive" "reminder" replyTo subject body
  where replyTo = Text.intercalate "+" ["snooze", keyShow messageId]
        subject = fromMaybe "Reminder!" (messageSubject message)
        body = messageBody message

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "mailer.conf", Conf.Optional "shared.conf"]
  connStr <- Conf.require conf "conn"
  mandrillKey <- Conf.require conf "mandrill-key"
  domain <- Conf.require conf "mail-domain"
  let mailer = Mailer mandrillKey domain

  withDismissiveIO connStr $ do
    messages <- unsentMessages
    liftIO $ putStrLn $ mconcat ["sending ", show (length messages), " messages"]
    for_ messages $ \(message, user) -> do
      let email = emailForMessage (message, user)
      runEitherT $ runReaderT (sendMail email) mailer
      markSent (entityKey message)
