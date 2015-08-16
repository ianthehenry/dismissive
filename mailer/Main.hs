import BasePrelude hiding (insert)
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Dismissive.Api
import qualified Data.Configurator as Conf
import Mailer
import Dismissive.Types

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "mailer.conf", Conf.Optional "shared.conf"]
  connStr <- Conf.require conf "conn"
  mandrillKey <- Conf.require conf "mandrill-key"
  domain <- Conf.require conf "mail-domain"

  let from = "reminder@" <> domain
  let replyTo = "snooze@" <> domain

  let email = Email "user@example.com"
                    "Dismissive"
                    from
                    replyTo
                    "example subject"
                    "example body"
  let mailer = newMailer mandrillKey

  withDismissiveIO connStr $ do
    result <- runEitherT $ runReaderT (sendMail email) mailer
    liftIO $ print result
    liftIO $ putStrLn "done"
