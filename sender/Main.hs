import BasePrelude hiding (left)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import Control.Monad.Trans.Either
import Control.Monad.Logger
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Control.Monad.Reader
import Dismissive.Api
import qualified Data.Configurator as Conf
import Dismissive.Mailer
import Dismissive.Types

emailForReminder :: (Entity Reminder, Entity User) -> Token -> LocalEmail
emailForReminder ( Entity { entityVal = reminder }
                 , Entity { entityVal = User { userEmail = toEmail } }
                 ) snoozeToken =
  LocalEmail toEmail "Dismissive" ("reminder", Nothing) (replyTo, Just "app") "Reminder!" body
  where replyTo = Text.intercalate "-" ["snooze", hexify snoozeToken]
        body = reminderBody reminder
        hexify = Text.decodeUtf8 . B16.encode

liftEither :: Monad m => Either a b -> EitherT () m b
liftEither (Right x) = return x
liftEither _ = left ()

main :: IO ()
main = do
  conf <- Conf.load [Conf.Optional "sender.conf", Conf.Optional "shared.conf"]
  connStr <- Conf.require conf "conn"
  mailer <- MailerConf <$> Conf.require conf "mandrill-key"
                       <*> Conf.require conf "mail-domain"

  runStderrLoggingT $ withDismissiveIO connStr $ \runDismissive -> runDismissive $ do
    reminders <- unsentReminders
    liftIO $ putStrLn $ mconcat ["sending ", show (length reminders), " reminders"]
    for_ reminders $ \(reminder, user) -> runEitherT $ do
      let reminderId = entityKey reminder
      let userId = entityKey user
      snoozeToken <- liftEither =<< lift (createToken [PermissionEdit reminderId] userId)
      let email = emailForReminder (reminder, user) snoozeToken
      liftEither =<< runMailerT mailer (sendMail email)
      lift $ markSent (entityKey reminder)
