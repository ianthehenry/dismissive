import BasePrelude hiding (insert)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Reader
import Dismissive.Api
import qualified Data.Configurator as Conf
import Mailer
import Dismissive.Types

emailForReminder :: (Entity Reminder, Entity User) -> LocalEmail
emailForReminder ( Entity { entityKey = reminderId, entityVal = reminder }
                 , Entity { entityVal = User { userEmail = toEmail } }
                 ) =
  LocalEmail toEmail "Dismissive" "reminder" replyTo "Reminder!" body
  where replyTo = Text.intercalate "+" ["snooze", keyShow reminderId]
        body = reminderBody reminder

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "mailer.conf", Conf.Optional "shared.conf"]
  connStr <- Conf.require conf "conn"
  mandrillKey <- Conf.require conf "mandrill-key"
  domain <- Conf.require conf "mail-domain"
  let mailer = Mailer mandrillKey domain

  withDismissiveIO connStr $ do
    reminders <- unsentReminders
    liftIO $ putStrLn $ mconcat ["sending ", show (length reminders), " reminders"]
    for_ reminders $ \(reminder, user) -> do
      let email = emailForReminder (reminder, user)
      runReaderT (sendMail email) mailer
      markSent (entityKey reminder)
