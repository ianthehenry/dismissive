{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger
import Control.Monad.Trans.Either
import qualified Data.Configurator as Conf
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString.Base16 as B16
import Dismissive.Api
import Dismissive.Servant
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

server' :: Dismiss -> Server Api
server' nat = enter nat server

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "receiver.conf", Conf.Optional "shared.conf"]
  port <- Conf.require conf "port"
  connStr <- Conf.require conf "conn"

  putStrLn ("listening on port " <> show port)

  runStderrLoggingT $ getDismiss connStr $ \nat ->
    let application = serve api (server' nat)
     in liftIO (run port application)
