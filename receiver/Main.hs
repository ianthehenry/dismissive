{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Data.Time.Clock
import Data.Text (Text)
import Dismissive.Api
import Dismissive.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types

type Api = ReqBody '[FormUrlEncoded] [MandrillEvent] :> Post '[JSON] ()

api :: Proxy Api
api = Proxy

reminderify :: Message -> DismissiveIO (Maybe Reminder)
reminderify Message { subject, from, body } = do
  maybeSender <- getUser from
  case maybeSender of
    Nothing -> return Nothing
    Just sender -> do
      now <- liftIO getCurrentTime
      let reminder = Reminder body now False (entityKey sender)
      liftIO (print reminder)
      return (Just reminder)

type Dismiss = DismissiveT (EitherT ServantErr IO) :~> EitherT ServantErr IO

dismissiveToEither :: Monad m => DismissiveT m Dismiss
dismissiveToEither = do
  pool <- ask
  return $ Nat (`runDismissiveT` pool)

handleMessage :: Text -> Message -> DismissiveIO ()
handleMessage "inbound" message = do
  maybeReminder <- reminderify message
  maybe (return ()) insertReminder maybeReminder
handleMessage _ _ = return ()

uncurryEvent :: (Text -> Message -> a) -> MandrillEvent -> a
uncurryEvent f MandrillEvent { event, msg } = f event msg

server :: ServerT Api (DismissiveT (EitherT ServantErr IO))
server events = for_ events (uncurryEvent handleMessage)

server' :: DismissiveIO (Server Api)
server' = flip enter server <$> dismissiveToEither

application :: DismissiveIO Application
application = serve api <$> server'

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "receiver.conf", Conf.Optional "shared.conf"]
  port <- Conf.require conf "port"
  connStr <- Conf.require conf "conn"

  putStrLn ("listening on port " <> show port)
  withDismissiveIO connStr $ do
    app <- application
    liftIO (run port app)
