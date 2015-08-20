{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString.Base16 as B16
import Dismissive.Api
import Dismissive.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types

type Api = ReqBody '[FormUrlEncoded] [MandrillEvent] :> Post '[JSON] ()

api :: Proxy Api
api = Proxy

extractToken :: Message -> Token
extractToken = fst . B16.decode . Text.encodeUtf8 . head . Text.split (== '@') . messageTo

type Dismiss = DismissiveT (EitherT ServantErr IO) :~> EitherT ServantErr IO

dismissiveToEither :: Monad m => DismissiveT m Dismiss
dismissiveToEither = do
  pool <- ask
  rng <- get
  return $ Nat (runDismissiveT rng pool)

handleMessage :: Text -> Message -> DismissiveIO ()
handleMessage "inbound" message = do
  let token = extractToken message
  now <- liftIO getCurrentTime
  void $ addReminder token now (messageBody message)
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
